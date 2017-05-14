module Typecheck (typecheck, itemIdent) where

import AbsStarsepLang
import Control.Monad
import Control.Monad.RWS (RWST, ask, get, put, lift, runRWST)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Errors
import PrintStarsepLang (printTree)

type TypedFnDefs = Map Ident Type
type TCEnv = (TypedFnDefs, Type)
type TCIdentState = Map Ident (Bool, Type)
type TCDeclState = [Ident]
type TCState = (TCIdentState, TCDeclState)
type TCMonad = RWST TCEnv () TCState IO

getState :: TCMonad TCIdentState
getState = do
  (s, _) <- get
  return s

putState :: TCIdentState -> TCMonad ()
putState newState = do
  decl <- getDecl
  put (newState, decl)

getDecl :: TCMonad TCDeclState
getDecl = do
  (_, decl) <- get
  return decl

addDecl :: Ident -> TCMonad ()
addDecl ident = do
  (s, decl) <- get
  when (ident `elem` decl) $ lift $ Errors.alreadyDecl ident
  put (s, ident : decl)

typeOf :: Expr -> TCMonad Type
typeOf q =
  case q of
    EFunExec (FunExec ident exprs) -> outputType ident exprs
    EList t exprs -> do
      forM_ exprs (`assertType` t)
      return $ ListT t
    EJoin a b -> do
      ta <- listType a
      tb <- listType b
      when (ta /= tb) $ lift $ Errors.listDiffJoin a b ta tb
      return ta
    EAppend e l -> do
      t <- listType l
      let (ListT elemType) = t
      assertType e elemType
      return t
    EVar ident -> typeOfIdent ident
    EInt _ -> return Int
    EChar _ -> return Char
    EFloat _ -> return Float
    EString _ -> return $ ListT Char
    EFalse -> return Bool
    ETrue -> return Bool
    ENeg expr -> checkNeg expr
    ENot expr -> do
      assertBExpr expr
      return Bool
    EAdd e1 op e2 -> checkNumOp e1 e2 (printTree op)
    EMul e1 mulOp e2 -> do
      when (mulOp == Mod) $ do
        assertType e1 Int
        assertType e2 Int
      checkNumOp e1 e2 (printTree mulOp)
    ERel e1 op e2 -> do
      checkRelOp e1 e2 (printTree op)
      return Bool
    EOr b1 b2 -> checkBExprOp b1 b2
    EAnd b1 b2 -> checkBExprOp b1 b2
    ETernary b e1 e2 -> do
      assertBExpr b
      t <- typeOf e1
      assertType e2 t
      return t

listType :: Expr -> TCMonad Type
listType expr = do
  t <- typeOf expr
  case t of
    ListT _ -> return t
    _ -> do
      lift $ Errors.nonListType expr t
      return t

typeOfFun :: Ident -> TCMonad Type
typeOfFun ident = do
  (typed, _) <- ask
  return $ typed ! ident

typeOfVar :: Ident -> TCMonad Type
typeOfVar ident = do
  state <- getState
  assertVarDeclared ident
  let (_, t) = state ! ident
  return t

typeOfIdent :: Ident -> TCMonad Type
typeOfIdent ident = do
  (typed, _) <- ask
  if Map.member ident typed then
    typeOfFun ident
  else
    typeOfVar ident

checkBExprOp :: Expr -> Expr -> TCMonad Type
checkBExprOp b1 b2 = do
  assertBExpr b1
  assertBExpr b2
  return Bool

checkBinOp :: Expr -> Expr -> String -> TCMonad Type
checkBinOp expr1 expr2 name = do
  t1 <- typeOf expr1
  t2 <- typeOf expr2
  unless (t1 == t2) $ lift $ Errors.diffTypesBinOp name t1 t2
  return t1

assertNumericExpr :: Expr -> TCMonad ()
assertNumericExpr expr = do
  t <- typeOf expr
  case t of
    Int -> return ()
    Float -> return ()
    _ -> lift $ Errors.nonNumeric expr t

iterableElemType :: Expr -> TCMonad Type
iterableElemType expr = do
  t <- typeOf expr
  case t of
    Str -> return Char
    ListT e -> return e
    _ -> do
      lift $ Errors.nonIterable expr t
      return Int

checkNeg :: Expr -> TCMonad Type
checkNeg expr = do
  assertNumericExpr expr
  typeOf expr

checkNumOp :: Expr -> Expr -> String -> TCMonad Type
checkNumOp expr1 expr2 name = do
  assertNumericExpr expr1
  assertNumericExpr expr2
  checkBinOp expr1 expr2 name

checkArgs :: Ident -> [Expr] -> [Type] -> TCMonad Type
checkArgs ident args t = do
  let nArgs = length args
      expected = length t - 1
  when (nArgs /= expected) $ lift $ Errors.numberOfArgs ident nArgs expected
  argsTypes <- mapM typeOf args
  let types = init t
  when (argsTypes /= types) $ lift $ Errors.typesOfArgs ident argsTypes types
  return $ last t

outputTypeFun :: Ident -> [Expr] -> TCMonad Type
outputTypeFun ident args = do
  (typed, _) <- ask
  let (FnType types) = typed ! ident
  checkArgs ident args types

outputTypeVarFun :: Ident -> [Expr] -> TCMonad Type
outputTypeVarFun ident args = do
  (state, _) <- get
  let (_, FnType types) = state ! ident
  checkArgs ident args types

outputType :: Ident -> [Expr] -> TCMonad Type
outputType ident args = do
  (typed, _) <- ask
  state <- getState
  if Map.member ident typed then
    outputTypeFun ident args
  else if Map.member ident state then
    outputTypeVarFun ident args
  else do
    _ <- lift $ Errors.functionUndeclared ident
    return Int

fnHeaderToFnType :: Type -> [Arg] -> Type
fnHeaderToFnType outType args =
  FnType $ map (\arg -> case arg of Arg t _ -> t) args ++ [outType]

addTypedFnDef :: TypedFnDefs -> FnDef -> IO TypedFnDefs
addTypedFnDef typed (FnDef outType ident args _) = do
  when (Map.member ident typed) $ Errors.multipleFnDef ident
  return $ Map.insert ident (fnHeaderToFnType outType args) typed

assertCorrectMain :: TypedFnDefs -> IO ()
assertCorrectMain typedFns = do
  when (Map.notMember (Ident "main") typedFns) Errors.noMain
  case typedFns ! Ident "main" of
    FnType [Void] -> return ()
    _ -> Errors.badMain

checkShadow :: Ident -> TCMonad ()
checkShadow ident = do
  (typed, _) <- ask
  state <- getState
  when (Map.member ident typed) $ lift $ Errors.shadowTopDef ident
  when (Map.member ident state) $ lift $ Errors.shadowVariable ident

assertType :: Expr -> Type -> TCMonad ()
assertType expr t = do
  typeof <- typeOf expr
  let t1 = if t == Str then ListT Char else t
  when (t1 /= typeof) $ lift $ Errors.expectedExpression expr typeof t1

assertVarDeclared :: Ident -> TCMonad ()
assertVarDeclared ident = do
  state <- getState
  when (Map.notMember ident state) $ lift $ Errors.variableUndeclared ident

assertNonConst :: Ident -> TCMonad ()
assertNonConst ident = do
  state <- getState
  assertVarDeclared ident
  let (isConst, _) = state ! ident
  when isConst $ lift $ Errors.changingConst ident

itemIdent :: Item -> Ident
itemIdent item = case item of
  Init ident _ -> ident
  NoInit ident -> ident

typecheckDecl :: Item -> Type -> TCMonad ()
typecheckDecl item t1 = do
  let t = if t1 == Str then ListT Char else t1
  let ident = itemIdent item
  case item of
    NoInit _ -> case t of
       FnType _ -> lift $ Errors.funNoInit ident t
       _ -> return ()
    Init _ expr -> assertType expr t
  state <- getState
  addDecl ident
  checkShadow ident
  putState (Map.insert (itemIdent item) (False, t) state)

typecheckIncr :: Ident -> TCMonad ()
typecheckIncr ident = do
  assertVarDeclared ident
  assertType (EVar ident) Int

typecheckAuto :: Item -> Bool -> TCMonad()
typecheckAuto item isConst = do
  state <- getState
  let ident = itemIdent item
  checkShadow ident
  when (item == NoInit ident) $ lift $ Errors.noInit ident isConst
  let (Init _ expr) = item
  t <- typeOf expr
  putState (Map.insert ident (isConst, t) state)

typecheckAss :: Ident -> AssOp -> Expr -> TCMonad ()
typecheckAss ident assOp expr = do
  assertNonConst ident
  ltype <- typeOf $ EVar ident
  assertType expr ltype
  when (assOp /= Assign) $ assertNumericExpr expr
  when (assOp == ModAss) $ do
    assertType (EVar ident) Int
    assertType expr Int

typecheckOper :: Oper -> TCMonad ()
typecheckOper oper = do
  (_, returnType) <- ask
  void $ case oper of
    Decl t items -> forM_ items (`typecheckDecl` t)
    Let items -> forM_ items (`typecheckAuto` True)
    Auto items -> forM_ items (`typecheckAuto` False)
    Ass ident assOp expr -> typecheckAss ident assOp expr
    Incr ident -> typecheckIncr ident
    Decr ident -> typecheckIncr ident
    Ret expr -> do
      when (returnType == Void) $ lift $ Errors.retVoid expr
      t <- typeOf expr
      when (returnType /= t) $ lift $ Errors.badRetType expr t returnType
    VRet -> when (returnType /= Void) $ lift $ Errors.vRetNoVoid returnType
    FnExec (FunExec ident args) -> void $ outputType ident args
    Print expr -> assertPrintable expr
    Assert bExpr -> assertBExpr bExpr

assertPrintable :: Expr -> TCMonad ()
assertPrintable expr = do
  t <- typeOf expr
  case t of
    Void -> lift $ Errors.nonPrintable expr t
    FnType _ -> lift $ Errors.nonPrintable expr t
    _ -> return ()

assertComparable :: Expr -> TCMonad ()
assertComparable expr = do
  t <- typeOf expr
  case t of
    FnType _ -> lift $ Errors.nonComparable expr t
    _ -> return ()

checkRelOp :: Expr -> Expr -> String -> TCMonad ()
checkRelOp expr1 expr2 name = do
  assertComparable expr1
  void $ checkBinOp expr1 expr2 name

assertBExpr :: Expr -> TCMonad ()
assertBExpr bExpr = do
  t <- typeOf bExpr
  case t of
    Bool -> return ()
    _ -> lift $ Errors.nonBoolean bExpr

typecheckIfStmt :: IfStmt -> TCMonad ()
typecheckIfStmt ifStmt =
  case ifStmt of
    IfStmt expr block -> do
      assertBExpr expr
      typecheckBlock block
    IfElifStmt nextIf expr block -> do
      assertBExpr expr
      typecheckBlock block
      typecheckIfStmt nextIf

typecheckIfElseStmt :: IfElseStmt -> TCMonad ()
typecheckIfElseStmt (IfElseStmt ifStmt block) = do
  typecheckIfStmt ifStmt
  typecheckBlock block

typecheckStmt :: Stmt -> TCMonad ()
typecheckStmt stmt =
  case stmt of
    BStmt block -> typecheckBlock block
    OperStmt o -> typecheckOper o
    While bExpr block -> do
      assertBExpr bExpr
      typecheckBlock block
    For o1 b o2 block -> do
      typecheckOper o1
      assertBExpr b
      typecheckOper o2
      typecheckBlock block
    Foreach ident iter block -> do
      elemType <- iterableElemType iter
      typecheckDecl (NoInit ident) elemType
      typecheckBlock block
    Loop block -> typecheckBlock block
    CondIf i -> typecheckIfStmt i
    ElseStmt i -> typecheckIfElseStmt i

typecheckBlock :: Block -> TCMonad ()
typecheckBlock (Block stmts) = do
  (state, decl) <- get
  put (state, [])
  forM_ stmts typecheckStmt
  put (state, decl)

addFunctionArgToState :: TCIdentState -> Arg -> IO TCIdentState
addFunctionArgToState state (Arg t arg) = do
  when (Map.member arg state) $ Errors.sameArgNames arg
  return $ Map.insert arg (False, t) state

typecheckFunction :: FnDef -> TypedFnDefs -> IO ()
typecheckFunction (FnDef outType i args body) typed = do
  funState <- foldM addFunctionArgToState Map.empty args
  void $ runRWST (typecheckBlock body) (typed, outType) (funState, [])
  when ((outType /= Void) && not (isReturning (BStmt body))) $
    Errors.notReturning i

isReturning :: Stmt -> Bool
isReturning stmt = case stmt of
  BStmt (Block s) -> any isReturning s
  OperStmt oper -> isReturningOper oper
  For oper _ _ _ -> isReturningOper oper
  ElseStmt elseStmt -> isReturningElseStmt elseStmt
  _ -> False

isReturningOper :: Oper -> Bool
isReturningOper oper = case oper of
  Ret _ -> True
  _ -> False

isReturningIfStmt :: IfStmt -> Bool
isReturningIfStmt ifStmt = case ifStmt of
  IfStmt _ b -> isReturning $ BStmt b
  IfElifStmt iS _ b -> isReturning (BStmt b) && isReturningIfStmt iS

isReturningElseStmt :: IfElseStmt -> Bool
isReturningElseStmt (IfElseStmt ifStmt b) =
  isReturningIfStmt ifStmt && isReturning (BStmt b)

typecheck :: Program -> IO ()
typecheck (Program fns) = do
  typedFns <- foldM addTypedFnDef Map.empty fns
  assertCorrectMain typedFns
  forM_ fns (`typecheckFunction` typedFns)
