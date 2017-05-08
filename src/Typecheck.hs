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
      EInt _ -> return Int
      EFloat _ -> return Float
      EString _ -> return Str
      EFalse -> return Bool
      ETrue -> return Bool
      EVar ident -> do
        checkVarDeclared ident
        let Just (_, t) = Map.lookup ident state
        return t
      EAdd e1 addOp e2 -> checkAddOp e1 addOp e2
      EMul e1 mulOp e2 -> checkMulOp e1 mulOp e2
      EFun ident exprs -> outputTypeFun ident exprs
      _ -> fail $ "typeOf unimplemented for " ++ show expr

  checkBinOp :: Expr -> Expr -> TypecheckMonad Type
  checkBinOp expr1 expr2 = do
    t1 <- typeOf expr1
    t2 <- typeOf expr2
    when (t1 /= t2) $ lift $ Errors.diffTypesBinOp t1 t2
    return t1

  checkAddOp :: Expr -> AddOp -> Expr -> TypecheckMonad Type
  checkAddOp expr1 addOp expr2 = do
    checkBinOp expr1 expr2

  checkMulOp :: Expr -> MulOp -> Expr -> TypecheckMonad Type
  checkMulOp expr1 mulOp expr2 = do
      checkBinOp expr1 expr2

  outputTypeFun :: Ident -> [Expr] -> TypecheckMonad Type
  outputTypeFun ident exprs = do
    (typed, _) <- ask
    case Map.lookup ident typed of
      Just t -> return t
      Nothing -> fail "outputTypeFun unimplemented"

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
      VRet -> when (returnType /= Void) $ lift $ Errors.vRetNoVoid returnType
      Ret expr -> do
        when (returnType == Void) $ lift $ Errors.retVoid expr
        t <- typeOf expr
        when (returnType /= t) $ lift $ Errors.badRetType expr t returnType
      Decl t items -> forM_ items (`typecheckDecl` t)
      Incr ident -> typecheckIncr ident
      Decr ident -> typecheckIncr ident
      Let items -> typecheckLets items
      Ass ident assOp expr -> typecheckAss ident assOp expr
      _ -> fail $ "typecheckOper unimplemented for " ++ show oper
    return ()

  typecheckERel :: Expr -> RelOp -> Expr -> TypecheckMonad ()
  typecheckERel expr1 relOp expr2 = do
    type1 <- typeOf expr1
    type2 <- typeOf expr2
    return ()

  typecheckBExpr :: Expr -> TypecheckMonad ()
  typecheckBExpr bExpr = do
    case bExpr of
      EFalse -> return ()
      ETrue -> return ()
      ERel expr1 relOp expr2 -> typecheckERel expr1 relOp expr2
      _ -> fail $ "typecheckBExpr unimplemented for " ++ show bExpr

  typecheckIfStmt :: IfStmt -> TypecheckMonad ()
  typecheckIfStmt ifStmt = do
    case ifStmt of IfStmt expr block -> do {
      typecheckBExpr expr;
      typecheckBlock block
    }
    lift $ print ifStmt
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

  typecheckFunction :: FnDef -> TypedFnDefs -> IO ()
  typecheckFunction (FnDef outputType ident args body) typed = do
    runRWST (typecheckBlock body) (typed, outputType) Map.empty
    return ()

  typecheck :: Program -> IO ()
  typecheck (Program fns) = do
    typedFns <- foldM addTypedFnDef Map.empty fns
    checkMain typedFns
    forM_ fns (`typecheckFunction` typedFns)
    return ()
