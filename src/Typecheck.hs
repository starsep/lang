module Typecheck (typecheck) where
  import AbsStarsepLang
  import qualified Data.Map as Map
  import qualified Errors
  import Control.Monad
  import Control.Monad.RWS (RWST, ask, get, put, lift, runRWST)
  import Data.Maybe

  type TypedFnDefs = Map.Map Ident Type
  type TCEnv = (TypedFnDefs, Type)
  type TCVarState = Map.Map Ident (Bool, Type)
  type TCDeclState = [Ident]
  type TCState = (TCVarState, TCDeclState)
  type TCMonad = RWST TCEnv () TCState IO

  getState :: TCMonad (Map.Map Ident (Bool, Type))
  getState = do
    (state, _) <- get
    return state

  putState :: Map.Map Ident (Bool, Type) -> TCMonad ()
  putState newState = do
    (_, l) <- get
    put (newState, l)

  typeOf :: Expr -> TCMonad Type
  typeOf q =
    case q of
      EFun (FunExec ident exprs) -> outputType ident exprs
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
      EString _ -> return Str
      EFalse -> return Bool
      ETrue -> return Bool
      ENeg expr -> checkNeg expr
      ENot expr -> do
        assertBExpr expr
        return Bool
      EAdd e1 _ e2 -> checkNumOp e1 e2
      EMul e1 mulOp e2 -> do
        when (mulOp == Mod) $ do
          assertType e1 Int
          assertType e2 Int
        checkNumOp e1 e2
      ERel e1 _ e2 -> do
        checkRelOp e1 e2
        return Bool
      EOr b1 b2 -> checkBExprOp b1 b2
      EAnd b1 b2 -> checkBExprOp b1 b2
      ETernary b e1 e2 -> do
        assertBExpr b
        checkRelOp e1 e2
        typeOf e1
      Lambda args expr -> typeOfLambda args expr


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
    return $ fromJust $ Map.lookup ident typed

  typeOfVar :: Ident -> TCMonad Type
  typeOfVar ident = do
    state <- getState
    assertVarDeclared ident
    let Just (_, t) = Map.lookup ident state
    return t

  typeOfIdent :: Ident -> TCMonad Type
  typeOfIdent ident = do
    (typed, _) <- ask
    if Map.member ident typed then
      typeOfFun ident
    else
      typeOfVar ident

  typeOfLambda :: [Arg] -> Expr -> TCMonad Type
  typeOfLambda args expr = do
    state <- getState
    lambdaState <- lift $ foldM addFunctionArgToState state args
    putState lambdaState
    outType <- typeOf expr
    putState state
    return $ fnHeaderToFnType outType args

  checkBExprOp :: Expr -> Expr -> TCMonad Type
  checkBExprOp b1 b2 = do
    assertBExpr b1
    assertBExpr b2
    return Bool

  checkBinOp :: Expr -> Expr -> TCMonad Type
  checkBinOp expr1 expr2 = do
    t1 <- typeOf expr1
    t2 <- typeOf expr2
    unless (t1 == t2) $ lift $ Errors.diffTypesBinOp t1 t2
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

  checkNumOp :: Expr -> Expr -> TCMonad Type
  checkNumOp expr1 expr2 = do
    assertNumericExpr expr1
    assertNumericExpr expr2
    checkBinOp expr1 expr2

  checkArgs :: Ident -> [Expr] -> [Type] -> TCMonad Type
  checkArgs ident args t = do
    let nArgs = length args
        expected = length t - 1
    when (nArgs /= expected) $ lift $ Errors.numberOfArgs ident nArgs expected
    argsTypes <- mapM typeOf args
    let types = take expected t
    when (argsTypes /= types) $ lift $ Errors.typesOfArgs ident argsTypes types
    return $ last t

  outputTypeLambda :: Ident -> [Expr] -> TCMonad Type
  outputTypeLambda ident args = do
    state <- getState
    let Just (_, t) = Map.lookup ident state
    case t of
      FnType types -> checkArgs ident args types
      _ -> do
        lift $ Errors.notLambda ident
        return Int

  outputTypeFun :: Ident -> [Expr] -> TCMonad Type
  outputTypeFun ident args = do
    (typed, _) <- ask
    let Just (FnType types) = Map.lookup ident typed
    checkArgs ident args types

  outputType :: Ident -> [Expr] -> TCMonad Type
  outputType ident args = do
    (typed, _) <- ask
    state <- getState
    if Map.member ident typed then
      outputTypeFun ident args
    else if Map.member ident state then
      outputTypeLambda ident args
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
    case Map.lookup (Ident "main") typedFns of
      Just (FnType [Void]) -> return ()
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
    when (t /= typeof) $ lift $ Errors.expectedExpression expr typeof t

  assertVarDeclared :: Ident -> TCMonad ()
  assertVarDeclared ident = do
    state <- getState
    when (Map.notMember ident state) $ lift $ Errors.variableUndeclared ident

  assertNonConst :: Ident -> TCMonad ()
  assertNonConst ident = do
    state <- getState
    assertVarDeclared ident
    let Just (isConst, _) = Map.lookup ident state
    when isConst $ lift $ Errors.changingConst ident

  itemIdent :: Item -> Ident
  itemIdent item = case item of
    Init ident _ -> ident
    NoInit ident -> ident

  typecheckDecl :: Item -> Type -> TCMonad ()
  typecheckDecl item t = do
    case item of
      NoInit ident -> checkShadow ident
      Init ident expr -> do
        checkShadow ident
        assertType expr t
    state <- getState
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

  checkRelOp :: Expr -> Expr -> TCMonad ()
  checkRelOp expr1 expr2 = do
    assertComparable expr1
    void $ checkBinOp expr1 expr2

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
  typecheckBlock (Block stmts) =
    forM_ stmts typecheckStmt

  addFunctionArgToState :: TCVarState -> Arg -> IO TCVarState
  addFunctionArgToState state (Arg t arg) = do
    when (Map.member arg state) $ Errors.sameArgNames arg
    return $ Map.insert arg (False, t) state

  typecheckFunction :: FnDef -> TypedFnDefs -> IO ()
  typecheckFunction (FnDef outType _ args body) typed = do
    funState <- foldM addFunctionArgToState Map.empty args
    void $ runRWST (typecheckBlock body) (typed, outType) (funState, [])

  typecheck :: Program -> IO ()
  typecheck (Program fns) = do
    typedFns <- foldM addTypedFnDef Map.empty fns
    assertCorrectMain typedFns
    forM_ fns (`typecheckFunction` typedFns)
