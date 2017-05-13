module Interpreter (interpret) where

import AbsStarsepLang
import Control.Monad
import Control.Monad.IfElse
import Control.Monad.RWS (RWST, ask, get, put, lift, runRWST)
import Data.Maybe
import Numeric
import qualified Errors

type IEnv = ()
type IState = ()
--type IMonad = RWST IEnv () IState IO
type IMonad = IO

failure :: Show a => a -> IMonad ()
failure x = fail $ "Undefined case: " ++ show x

isMain :: FnDef -> Bool
isMain (FnDef _ (Ident name) _ _) = name == "main"

interpret :: Program -> IO ()
interpret (Program fns) = do
  let main = head $ filter isMain fns
  void $ transFnDef main

transFnDef :: FnDef -> IMonad ()
transFnDef (FnDef type_ ident args block) =
  -- TODO: set args values
  transBlock block

transBlock :: Block -> IMonad ()
transBlock (Block stmts) =
  forM_ stmts transStmt

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
    case first of
      Nothing -> return ()
      Just v -> do
        transOper (Auto [Init ident v])
  Loop block -> transStmt $ While ETrue block
  CondIf ifstmt -> void $ transIfStmt ifstmt
  ElseStmt ifelsestmt -> transIfElseStmt ifelsestmt



execPrintList :: Type -> [Expr] -> IMonad ()
execPrintList t l = forM_ l execPrint

execPrint :: Expr -> IMonad ()
execPrint expr = do
  value <- eval expr
  case value of
    EString s -> putStr s
    EInt i -> putStr $ show i
    EChar c -> putStr [c]
    EFloat f -> putStr $ Numeric.showFFloat (Just 4) f ""
    ETrue -> putStr $ show True
    EFalse -> putStr $ show False
    EList t l -> execPrintList t l
    _ -> fail "print :<"

execAssert :: Expr -> IMonad ()
execAssert expr = do
  b <- eval expr
  when (b /= ETrue) $ Errors.assert expr

transOper :: Oper -> IMonad ()
transOper x = case x of
  Decl type_ items -> failure x
  Let items -> failure x
  Auto items -> failure x
  Ass ident assop expr -> failure x
  Incr ident -> failure x
  Decr ident -> failure x
  Ret expr -> failure x
  VRet -> failure x
  FnExec funexec -> failure x
  Print expr -> execPrint expr
  Assert expr -> execAssert expr

toEBool :: Bool -> Expr
toEBool q = if q then ETrue else EFalse

eval :: Expr -> IO Expr
eval x =
  case x of
    EFun funexec -> fail $ show x
    EList type_ exprs -> return x
    EVar ident -> fail $ show x
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
    EJoin expr1 expr2 -> fail $ show x
    EAppend expr1 expr2 -> fail $ show x
    ERel expr1 relop expr2 -> do
      e1 <- eval expr1
      e2 <- eval expr2
      let op = transRelOp relop
      return $ toEBool $ e1 `op` e2
    EAnd expr1 expr2 -> do
      b1 <- eval expr1
      b2 <- eval expr2
      return $ if b1 == ETrue && b2 == ETrue then ETrue else EFalse
    EOr expr1 expr2 -> do
      b1 <- eval expr1
      b2 <- eval expr2
      return $ if b1 == ETrue || b2 == ETrue then ETrue else EFalse
    ETernary expr1 expr2 expr3 -> do
      b <- evalCond expr1
      if b then
        eval expr2
      else
        eval expr3
    Lambda args expr -> fail $ show x

transIdent :: Ident -> IMonad ()
transIdent x = case x of
  Ident string -> failure x
transArg :: Arg -> IMonad ()
transArg x = case x of
  Arg type_ ident -> failure x
transFunExec :: FunExec -> IMonad ()
transFunExec x = case x of
  FunExec ident exprs -> failure x
transItem :: Item -> IMonad ()
transItem x = case x of
  NoInit ident -> failure x
  Init ident expr -> failure x
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
transType :: Type -> IMonad ()
transType x = case x of
  Int -> failure x
  Char -> failure x
  Str -> failure x
  Bool -> failure x
  Float -> failure x
  Void -> failure x
  FnType types -> failure x
  ListT type_ -> failure x

transMathExpr :: Expr -> Expr -> (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> IMonad Expr
transMathExpr expr1 expr2 fni fnf = do
  e1 <- eval expr1
  e2 <- eval expr2
  case (e1, e2) of
    (EInt i1, EInt i2) -> return $ EInt $ i1 `fni` i2
    (EFloat f1, EFloat f2) -> return $ EFloat $ f1 `fnf` f2

floatExpr :: Expr -> Double
floatExpr (EFloat f) = f

intExpr :: Expr -> Integer
intExpr (EInt i) = i

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
transRelOp :: Ord a => RelOp -> (a -> a -> Bool)
transRelOp x = case x of
  LTH -> (<)
  LE -> (<=)
  GTH -> (>)
  GE -> (>=)
  EQU -> (==)
  NE -> (/=)
transAssOp :: AssOp -> IMonad ()
transAssOp x = case x of
  Assign -> failure x
  PlusAss -> failure x
  MinusAss -> failure x
  MulAss -> failure x
  DivAss -> failure x
  ModAss -> failure x
