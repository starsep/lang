module Interpreter (interpret) where

import AbsStarsepLang
import Control.Exception.Base
import Control.Monad
import Control.Monad.State (StateT, get, put, lift, runStateT)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe
import qualified Errors
import Numeric

type Loc = Int
type IEnv = Map.Map Ident Loc
type IVarState = Map.Map Loc Expr
type IShadowed = Map.Map Ident Loc
type IState = (Loc, IEnv, IVarState, IShadowed)
type IMonad = StateT IState IO

failure :: Show a => a -> IMonad ()
failure x = fail $ "Undefined case: " ++ show x

isMain :: FnDef -> Bool
isMain (FnDef _ (Ident name) _ _) = name == "main"

addFnToState :: FnDef -> IState -> IState
addFnToState (FnDef _ ident args _) (loc, env, state, s) =
  let newEnv = env --Map.insert ident loc
      newState = state in --Map.insert loc
  (loc + 1, newEnv, newState, s)

initState :: [FnDef] -> IState
initState fns = foldr addFnToState (0, Map.empty, Map.empty, Map.empty) fns

interpret :: Program -> IO ()
interpret (Program fns) = do
  let main = head $ filter isMain fns
  void $ runStateT (transFnDef main) (initState fns)

transFnDef :: FnDef -> IMonad ()
transFnDef (FnDef type_ ident args block) =
  -- TODO: set args values
  transBlock block

transBlock :: Block -> IMonad ()
transBlock (Block stmts) = do
  oldShadowed <- getShadowed
  forM_ stmts transStmt
  env <- ask
  shadowed <- getShadowed
  putShadowed oldShadowed
  putEnv $ Map.union shadowed env

ask :: IMonad IEnv
ask = do
  (_, env, _, _) <- get
  return env

getShadowed :: IMonad IShadowed
getShadowed = do
  (_, _, _, shadowed) <- get
  return shadowed

getState :: IMonad IVarState
getState = do
  (_, _, state, _) <- get
  return state

putState :: IVarState -> IMonad ()
putState newState = do
  (loc, env, _, s) <- get
  put (loc, env, newState, s)

putEnv :: IEnv -> IMonad ()
putEnv env = do
  (loc, _, state, shadowed) <- get
  put (loc, env, state, shadowed)

putShadowed :: IShadowed -> IMonad ()
putShadowed shadowed = do
  (loc, env, state, _) <- get
  put (loc, env, state, shadowed)

evalCond :: Expr -> IMonad Bool
evalCond e = do
  t <- eval e
  return $ t == ETrue

headList :: Expr -> IMonad (Maybe Expr)
headList expr = do
  e <- eval expr
  let (EList _ l) = e
  return $ listToMaybe l
transStmt :: Stmt -> IMonad ()
transStmt x = case x of
  BStmt block -> transBlock block
  OperStmt oper -> transOper oper
  While expr block -> do
    c <- evalCond expr
    when c $ do
      transBlock block
      transStmt x
  For oper1 expr oper2 (Block s) -> do
    let body = Block $ s ++ [OperStmt oper2]
    transBlock $ Block [OperStmt oper1, While expr body]
  Foreach ident expr block -> do
    first <- headList expr
    when (isJust first) $ transOper (Auto [Init ident (fromJust first)])
  Loop block -> transStmt $ While ETrue block
  CondIf ifstmt -> void $ transIfStmt ifstmt
  ElseStmt ifelsestmt -> transIfElseStmt ifelsestmt

execPrintList :: Type -> [Expr] -> IMonad ()
execPrintList t l = forM_ l execPrint

execPrint :: Expr -> IMonad ()
execPrint expr = do
  value <- eval expr
  case value of
    EString s -> lift $ putStr s
    EInt i -> lift $ putStr $ show i
    EChar c -> lift $ putStr [c]
    EFloat f -> lift $ putStr $ Numeric.showFFloat (Just 4) f ""
    ETrue -> lift $ putStr $ show True
    EFalse -> lift $ putStr $ show False
    EList t l -> execPrintList t l
    _ -> fail "print :<"

execAssert :: Expr -> IMonad ()
execAssert expr = do
  b <- eval expr
  when (b /= ETrue) $ lift $ Errors.assert expr


defaultValue :: Type -> Expr
defaultValue t = case t of
  Int -> EInt 0
  Char -> EChar '\0'
  Str -> EString ""
  Bool -> EFalse
  Float -> EFloat 0.0
  ListT t -> EList t []

getLoc :: Ident -> IMonad Loc
getLoc ident = do
  env <- ask
  return $ env ! ident

assign :: Ident -> Expr -> IMonad ()
assign ident expr = do
  e <- eval expr
  state <- getState
  loc <- getLoc ident
  putState $ Map.insert loc e state

declare :: Ident -> Expr -> IMonad ()
declare ident e = do
  expr <- eval e
  (loc, env, state, shadowed) <- get
  let newShadowed = if Map.member ident env then Map.insert ident (env ! ident) env else env
  put (loc + 1, Map.insert ident loc env, Map.insert loc expr state, newShadowed)

transDecl :: Type -> Item -> IMonad ()
transDecl t item =
  case item of
    Init ident expr -> declare ident expr
    NoInit ident -> declare ident $ defaultValue t

transLet :: Item -> IMonad ()
transLet (Init ident expr) = declare ident expr

transAssOp :: AssOp -> Ident -> Expr -> Expr
transAssOp x ident expr =
  let varExp = EVar ident in
  case x of
    Assign -> expr
    PlusAss -> EAdd varExp Plus expr
    MinusAss -> EAdd varExp Minus expr
    MulAss -> EMul varExp Times expr
    DivAss -> EMul varExp Div expr
    ModAss -> EMul varExp Mod expr

transOper :: Oper -> IMonad ()
transOper x = case x of
  Decl type_ items -> forM_ items $ transDecl type_
  Let items -> forM_ items transLet
  Auto items -> forM_ items transLet
  Ass ident assop expr -> do
    e <- eval $ transAssOp assop ident expr
    assign ident e
  Incr ident -> transOper $ Ass ident PlusAss $ EInt 1
  Decr ident -> transOper $ Ass ident MinusAss $ EInt 1
  Ret expr -> failure x
  VRet -> failure x
  FnExec funexec -> failure x
  Print expr -> execPrint expr
  Assert expr -> execAssert expr

toEBool :: Bool -> Expr
toEBool q = if q then ETrue else EFalse

eval :: Expr -> IMonad Expr
eval x =
  case x of
    EList type_ exprs -> return x
    EVar ident -> do
      state <- getState
      loc <- getLoc ident
      return $ state ! loc
    EInt integer -> return x
    EChar char -> return x
    EFloat double -> return x
    EString string -> return $ EList Char $ map EChar string
    EFalse -> return x
    ETrue -> return x
    ENeg expr -> do
      e <- eval expr
      case e of
        EInt i -> return $ EInt (-i)
        EFloat f -> return $ EFloat (-f)
    ENot expr -> do
      b <- eval expr
      case b of
        ETrue -> return EFalse
        EFalse -> return ETrue
    EMul expr1 mulop expr2 ->
      let i = transMulOpI mulop
          f = transMulOpF mulop in
      transMathExpr expr1 expr2 i f
    EAdd expr1 addop expr2 ->
      let i = transAddOp addop
          f = transAddOp addop in
      transMathExpr expr1 expr2 i f
    EJoin expr1 expr2 -> do
      el1 <- eval expr1
      el2 <- eval expr2
      let (EList t l1) = el1
          (EList _ l2) = el2
      return $ EList t $ l1 ++ l2
    EAppend expr1 expr2 -> do
      e <- eval expr1
      el <- eval expr2
      let (EList t l) = el
      return $ EList t (e : l)
    ERel expr1 relop expr2 -> do
      e1 <- eval expr1
      e2 <- eval expr2
      let op = transRelOp relop
      return $ toEBool $ e1 `op` e2
    EAnd expr1 expr2 -> do
      b1 <- eval expr1
      b2 <- eval expr2
      return $ toEBool $ b1 == ETrue && b2 == ETrue
    EOr expr1 expr2 -> do
      b1 <- eval expr1
      b2 <- eval expr2
      return $ toEBool $ b1 == ETrue || b2 == ETrue
    ETernary expr1 expr2 expr3 -> do
      b <- evalCond expr1
      if b then
        eval expr2
      else
        eval expr3
    Lambda args expr -> fail $ show x

transFunExec :: FunExec -> IMonad ()
transFunExec x@(FunExec ident exprs) = failure x

transIfStmt :: IfStmt -> IMonad Bool
transIfStmt x = case x of
  IfElifStmt ifstmt expr block -> do
    b <- transIfStmt ifstmt
    if b then
      return b
    else
      transIfStmt (IfStmt expr block)
  IfStmt expr block -> do
    b <- evalCond expr
    when b $ transBlock block
    return b

transIfElseStmt :: IfElseStmt -> IMonad ()
transIfElseStmt (IfElseStmt ifstmt block) = do
  b <- transIfStmt ifstmt
  unless b $ transBlock block

divZeroHandler :: ArithException -> IO Integer
divZeroHandler DivideByZero = Errors.divZero
divZeroHandler _ = return 0

tryIntOp :: Integer -> Integer -> (Integer -> Integer -> Integer) -> IO Integer
tryIntOp i1 i2 fni = evaluate $ i1 `fni` i2

transMathExpr :: Expr -> Expr -> (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> IMonad Expr
transMathExpr expr1 expr2 fni fnf = do
  e1 <- eval expr1
  e2 <- eval expr2
  case (e1, e2) of
    (EInt i1, EInt i2) -> do
      r <- lift $ tryIntOp i1 i2 fni `catch` divZeroHandler
      return $ EInt r
    (EFloat f1, EFloat f2) -> return $ EFloat $ f1 `fnf` f2

floatExpr :: Expr -> Double
floatExpr (EFloat f) = f

transAddOp :: Num a => AddOp -> (a -> a -> a)
transAddOp x = case x of
  Plus -> (+)
  Minus -> (-)
transMulOpI :: Integral a => Num a => MulOp -> (a -> a -> a)
transMulOpI x = case x of
  Times -> (*)
  Div -> div
  Mod -> mod
transMulOpF :: Fractional a => Num a => MulOp -> (a -> a -> a)
transMulOpF x = case x of
  Times -> (*)
  Div -> (/)
  Mod -> (/)
transRelOp :: Ord a => RelOp -> (a -> a -> Bool)
transRelOp x = case x of
  LTH -> (<)
  LE -> (<=)
  GTH -> (>)
  GE -> (>=)
  EQU -> (==)
  NE -> (/=)
