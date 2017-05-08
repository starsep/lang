module Typecheck (typecheck) where
  import AbsStarsepLang
  import ErrM
  import qualified Data.Map as Map
  import qualified Errors
  import Environment
  import Control.Monad
  import Control.Monad.RWS

  typeOf :: Expr -> TypecheckMonad Type
  typeOf expr = do
    state <- get
    case expr of
      EFun ident exprs -> outputTypeFun ident exprs
      EVar ident -> do
        checkVarDeclared ident
        let Just (_, t) = Map.lookup ident state
        return t
      EInt _ -> return Int
      EFloat _ -> return Float
      EString _ -> return Str
      EFalse -> return Bool
      ETrue -> return Bool
      ENeg expr -> checkNeg expr
      ENot expr -> do
        typecheckBExpr expr
        return Bool
      EAdd e1 addOp e2 -> checkAddOp e1 addOp e2
      EMul e1 mulOp e2 -> checkMulOp e1 mulOp e2
      ERel e1 relOp e2 -> do
        checkRelOp e1 relOp e2
        return Bool
      EOr b1 b2 -> checkBExprOp b1 b2
      EAnd b1 b2 -> checkBExprOp b1 b2
      ETernary b e1 e2 -> do
        typecheckBExpr b
        checkRelOp e1 EQU e2
        typeOf e1
      Lambda args expr ->
        checkLambda args expr

  checkLambda :: [Ident] -> Expr -> TypecheckMonad Type
  checkLambda args expr = typeOf expr

  checkBExprOp :: Expr -> Expr -> TypecheckMonad Type
  checkBExprOp b1 b2 = do
    typecheckBExpr b1
    typecheckBExpr b2
    return Bool

  checkBinOp :: Expr -> Expr -> TypecheckMonad Type
  checkBinOp expr1 expr2 = do
    t1 <- typeOf expr1
    t2 <- typeOf expr2
    unless (t1 == t2) $ lift $ Errors.diffTypesBinOp t1 t2
    return t1

  checkNumericExpr :: Expr -> TypecheckMonad ()
  checkNumericExpr expr = do
    t <- typeOf expr
    case t of
      Int -> return ()
      Float -> return ()
      _ -> lift $ Errors.nonNumeric expr

  checkNeg :: Expr -> TypecheckMonad Type
  checkNeg expr = do
    checkNumericExpr expr
    typeOf expr

  checkAddOp :: Expr -> AddOp -> Expr -> TypecheckMonad Type
  checkAddOp expr1 addOp expr2 = do
    checkBinOp expr1 expr2

  checkMulOp :: Expr -> MulOp -> Expr -> TypecheckMonad Type
  checkMulOp expr1 mulOp expr2 = do
      checkBinOp expr1 expr2

  checkFunExec :: Ident -> [Expr] -> TypecheckMonad ()
  checkFunExec ident args = do
    return ()

  outputTypeFun :: Ident -> [Expr] -> TypecheckMonad Type
  outputTypeFun ident args = do
    (typed, _) <- ask
    case Map.lookup ident typed of
      Just (FnType _ t) -> return t
      Nothing -> fail $ "outputTypeFun unimplemented for " ++ show ident

  fnHeaderToFnType :: Type -> [Arg] -> Type
  fnHeaderToFnType outputType args =
    FnType (map (\arg -> case arg of Arg t _ -> t) args) outputType

  addTypedFnDef :: TypedFnDefs -> FnDef -> IO TypedFnDefs
  addTypedFnDef typed (FnDef outputType ident args _) = do
    when (Map.member ident typed) $ Errors.multipleFnDef ident
    return $ Map.insert ident (fnHeaderToFnType outputType args) typed

  checkMain :: TypedFnDefs -> IO ()
  checkMain typedFns = do
    when (Map.notMember (Ident "main") typedFns) Errors.noMain
    case Map.lookup (Ident "main") typedFns of
      Just (FnType [] Void) -> return ()
      _ -> Errors.badMain

  checkShadow :: Ident -> TypecheckMonad ()
  checkShadow ident = do
    (typed, _) <- ask
    state <- get
    when (Map.member ident typed) $ lift $ Errors.shadowTopDef ident
    when (Map.member ident state) $ lift $ Errors.shadowVariable ident

  checkType :: Expr -> Type -> TypecheckMonad ()
  checkType expr t = do
    typeof <- typeOf expr
    when (t /= typeof) $ lift $ Errors.expectedExpression expr typeof t

  checkVarDeclared :: Ident -> TypecheckMonad ()
  checkVarDeclared ident = do
    state <- get
    when (Map.notMember ident state) $ lift $ Errors.variableUndeclared ident

  checkNonConst :: Ident -> TypecheckMonad ()
  checkNonConst ident = do
    state <- get
    checkVarDeclared ident
    let Just (isConst, _) = Map.lookup ident state
    when isConst $ lift $ Errors.changingConst ident

  itemIdent :: Item -> Ident
  itemIdent item = case item of
    Init ident _ -> ident
    NoInit ident -> ident

  typecheckDecl :: Item -> Type -> TypecheckMonad ()
  typecheckDecl item t = do
    case item of
      NoInit ident -> checkShadow ident
      Init ident expr -> do
        checkShadow ident
        checkType expr t
    state <- get
    put (Map.insert (itemIdent item) (False, t) state)

  typecheckIncr :: Ident -> TypecheckMonad ()
  typecheckIncr ident = do
    checkVarDeclared ident
    checkType (EVar ident) Int

  typecheckLet :: Item -> Type -> TypecheckMonad ()
  typecheckLet item expectedT = do
    state <- get
    let ident = itemIdent item
    when (item == NoInit ident) $ lift $ Errors.letNoInit ident
    let (Init _ expr) = item
    checkType expr expectedT
    put (Map.insert ident (True, expectedT) state)

  typecheckLets :: [Item] -> TypecheckMonad ()
  typecheckLets items = do
    let first = head items
        firstIdent = itemIdent first
    when (first == NoInit firstIdent) $ lift $ Errors.letNoInit firstIdent
    let (Init _ firstExpr) = first
    expectedT <- typeOf firstExpr
    forM_ items (`typecheckLet` expectedT)

  typecheckAss :: Ident -> AssOp -> Expr -> TypecheckMonad ()
  typecheckAss ident assOp expr = do
    checkNonConst ident

  typecheckOper :: Oper -> TypecheckMonad ()
  typecheckOper oper = do
    (_, returnType) <- ask
    case oper of
      Decl t items -> forM_ items (`typecheckDecl` t)
      Let items -> typecheckLets items
      Ass ident assOp expr -> typecheckAss ident assOp expr
      Incr ident -> typecheckIncr ident
      Decr ident -> typecheckIncr ident
      Ret expr -> do
        when (returnType == Void) $ lift $ Errors.retVoid expr
        t <- typeOf expr
        when (returnType /= t) $ lift $ Errors.badRetType expr t returnType
      VRet -> when (returnType /= Void) $ lift $ Errors.vRetNoVoid returnType
      FnExec ident args -> checkFunExec ident args
    return ()

  checkRelOp :: Expr -> RelOp -> Expr -> TypecheckMonad ()
  checkRelOp expr1 relOp expr2 = do
    _ <- checkBinOp expr1 expr2
    return ()

  typecheckBExpr :: Expr -> TypecheckMonad ()
  typecheckBExpr bExpr = do
    t <- typeOf bExpr
    case t of
      Bool -> return ()
      _ -> lift $ Errors.nonBoolean bExpr

  typecheckIfStmt :: IfStmt -> TypecheckMonad ()
  typecheckIfStmt ifStmt = do
    case ifStmt of
      IfStmt expr block -> do
        typecheckBExpr expr
        typecheckBlock block
      IfElifStmt nextIf expr block -> do
        typecheckBExpr expr
        typecheckBlock block
        typecheckIfStmt nextIf
    -- lift $ print ifStmt
    return ()

  typecheckStmt :: Stmt -> TypecheckMonad ()
  typecheckStmt stmt = do
    -- env <- ask
    -- state <- get
    case stmt of
      OperStmt o -> typecheckOper o
      CondIf i -> typecheckIfStmt i
      _ -> return ()
    -- lift $ print env
    -- lift $ print state
    -- lift $ print stmt
    return ()

  typecheckBlock :: Block -> TypecheckMonad ()
  typecheckBlock (Block stmts) = do
    forM_ stmts typecheckStmt
    return ()

  addFunctionArgToState :: TCState -> Arg -> IO TCState
  addFunctionArgToState state (Arg t arg) = do
    when (Map.member arg state) $ Errors.sameArgNames arg
    return $ Map.insert arg (False, t) state

  typecheckFunction :: FnDef -> TypedFnDefs -> IO ()
  typecheckFunction (FnDef outputType ident args body) typed = do
    funState <- foldM addFunctionArgToState Map.empty args
    runRWST (typecheckBlock body) (typed, outputType) funState
    return ()

  typecheck :: Program -> IO ()
  typecheck (Program fns) = do
    typedFns <- foldM addTypedFnDef Map.empty fns
    checkMain typedFns
    forM_ fns (`typecheckFunction` typedFns)
    return ()
