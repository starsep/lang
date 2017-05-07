module Typecheck (typecheck) where
  import AbsStarsepLang
  import ErrM
  import qualified Data.Map as Map
  import qualified Errors
  import Environment
  import Control.Monad
  import Control.Monad.RWS

  typeOf :: Expr -> TypecheckMonad Type
  -- TODO: implement
  typeOf expr = return Int

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

  typecheckOper :: Oper -> TypecheckMonad ()
  typecheckOper oper = do
    (_, returnType) <- ask
    case oper of
      VRet -> when (returnType /= Void) $ lift $ Errors.vRetNoVoid returnType
      Ret expr -> do
        when (returnType == Void) $ lift $ Errors.retVoid expr
        t <- typeOf expr
        when (returnType /= t) $ lift (Errors.badRetType expr t returnType)
      _ -> return ()
    lift $ print oper
    return ()

  typecheckBExpr :: Expr -> TypecheckMonad ()
  typecheckBExpr bExpr = return ()

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
