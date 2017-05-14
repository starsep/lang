module Interpreter (interpret) where

import AbsStarsepLang
import Control.Exception.Base
import Control.Monad
import Control.Monad.RWS (RWST, get, put, lift, ask, runRWST)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe
import qualified Errors
import Numeric
import PrintStarsepLang (printTree)

type Loc = Int
type IEnv = Map Ident FnDef
type IIdentState = Map Ident Loc
type IVarState = Map Loc Expr
type IShadowed = Map Ident Loc
type IReturn = (Bool, Expr)
type IState = (Loc, IIdentState, IVarState, IShadowed, IReturn)
type IMonad = RWST IEnv () IState IO

failure :: Show a => a -> IMonad ()
failure x = fail $ "Undefined case: " ++ show x

initState :: IState
initState = (0, Map.empty, Map.empty, Map.empty, (False, EFalse))

addFnToEnv :: FnDef -> IEnv -> IEnv
addFnToEnv fn@(FnDef _ ident _ _) = Map.insert ident fn

initEnv :: [FnDef] -> IEnv
initEnv = foldr addFnToEnv Map.empty

interpret :: Program -> IO ()
interpret (Program fns) = do
  let env = initEnv fns
      main = env ! Ident "main"
  void $ runRWST (transFnDef main []) (initEnv fns) initState

transFnDef :: FnDef -> [Expr] -> IMonad ()
transFnDef (FnDef type_ ident args block) es =
  -- TODO: set args values
  transBlock block

transBlock :: Block -> IMonad ()
transBlock (Block stmts) = do
  oldShadowed <- getShadowed
  forM_ stmts transStmt
  env <- getIdentState
  shadowed <- getShadowed
  putShadowed oldShadowed
  putIdentState $ Map.union shadowed env

getIdentState :: IMonad IIdentState
getIdentState = do
  (_, ident, _, _, _) <- get
  return ident

getState :: IMonad IVarState
getState = do
  (_, _, state, _, _) <- get
  return state

getShadowed :: IMonad IShadowed
getShadowed = do
  (_, _, _, shadowed, _) <- get
  return shadowed

getReturn :: IMonad IReturn
getReturn = do
  (_, _, _, _, ret) <- get
  return ret

putIdentState :: IIdentState -> IMonad ()
putIdentState iState = do
  (loc, _, state, shadowed, r) <- get
  put (loc, iState, state, shadowed, r)

putState :: IVarState -> IMonad ()
putState newState = do
  (loc, iState, _, s, r) <- get
  put (loc, iState, newState, s, r)

putShadowed :: IShadowed -> IMonad ()
putShadowed shadowed = do
  (loc, iState, state, _, r) <- get
  put (loc, iState, state, shadowed, r)

putReturn :: IReturn -> IMonad ()
putReturn ret = do
  (loc, iState, state, shadowed, _) <- get
  put (loc, iState, state, shadowed, ret)

evalCond :: Expr -> IMonad Bool
evalCond e = do
  t <- eval e
  return $ t == ETrue

listSplit :: Expr -> IMonad (Maybe Expr, [Expr], Type)
listSplit expr = do
  e <- eval expr
  let (EList t l) = e
  if null l then
    return (Nothing, [], Void)
  else
    return (Just $ head l, tail l, t)
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
  Foreach ident expr b@(Block stms) -> do
    (h, ta, t) <- listSplit expr
    when (isJust h) $ do
      transBlock $ Block (OperStmt (Auto [Init ident (fromJust h)]) : stms)
      transStmt $ Foreach ident (EList t ta) b
  Loop block -> transStmt $ While ETrue block
  CondIf ifstmt -> void $ transIfStmt ifstmt
  ElseStmt ifelsestmt -> transIfElseStmt ifelsestmt

execPrintList :: [Expr] -> IMonad ()
execPrintList l = forM_ l execPrint

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
    EList _ l -> execPrintList l
    _ -> fail "print :<"

execAssert :: Expr -> IMonad ()
execAssert expr = do
  b <- eval expr
  when (b /= ETrue) $ lift $ Errors.assert expr

dummyArg :: Type -> (Int, [Arg]) -> (Int, [Arg])
dummyArg t (i, args) = (i + 1, Arg t (Ident name) : args) where
  name = "dummyArg" ++ show i ++ printTree t

dummyLambdaArgs :: [Type] -> [Arg]
dummyLambdaArgs types = let (_, res) = foldr dummyArg (0, []) types in res

defaultValue :: Type -> Expr
defaultValue t = case t of
  Int -> EInt 0
  Char -> EChar '\0'
  Str -> EString ""
  Bool -> EFalse
  Float -> EFloat 0.0
  ListT ty -> EList ty []
  FnType types ->
    let output = last types
        args = dummyLambdaArgs $ init types
        expr = defaultValue output in
    Lambda args expr
  Void -> EFalse


getLoc :: Ident -> IMonad Loc
getLoc ident = do
  iState <- getIdentState
  return $ iState ! ident

assign :: Ident -> Expr -> IMonad ()
assign ident expr = do
  e <- eval expr
  state <- getState
  loc <- getLoc ident
  putState $ Map.insert loc e state

declare :: Ident -> Expr -> IMonad ()
declare ident e = do
  expr <- eval e
  (loc, iS, state, _, r) <- get
  let newShadowed = if Map.member ident iS then Map.insert ident (iS ! ident) iS else iS
  put (loc + 1, Map.insert ident loc iS, Map.insert loc expr state, newShadowed, r)

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

getFun :: Ident -> IMonad FnDef
getFun ident = do
  env <- ask
  return $ env ! ident

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
  Ret expr -> putReturn (True, expr)
  VRet -> transOper $ Ret EFalse
  FnExec (FunExec ident args) -> do
    f <- getFun ident
    transFnDef f args
  Print expr -> execPrint expr
  Assert expr -> execAssert expr

toEBool :: Bool -> Expr
toEBool q = if q then ETrue else EFalse

isFun :: Ident -> IMonad Bool
isFun ident = do
  env <- ask
  return $ Map.member ident env

eval :: Expr -> IMonad Expr
eval x =
  case x of
    EList t exprs -> do
      l <- mapM eval exprs
      return $ EList t l
    EVar ident -> do
      funVar <- isFun ident
      if funVar then
        return $ EVar ident
      else do
        state <- getState
        loc <- getLoc ident
        return $ state ! loc
    EInt _ -> return x
    EChar _ -> return x
    EFloat _ -> return x
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
      return $ toEBool $ b == EFalse
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
    Lambda args expr -> evalLambda args expr

evalLambda :: [Arg] -> Expr -> IMonad Expr
evalLambda args expr = return $ Lambda args expr

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
