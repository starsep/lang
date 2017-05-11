module Errors
  (parsing, typecheck, multipleFnDef, noMain, badMain, vRetNoVoid, retVoid,
   badRetType, expectedExpression, shadowTopDef, shadowVariable,
   variableUndeclared, noInit, changingConst, diffTypesBinOp,
   sameArgNames, nonNumeric, nonBoolean, functionUndeclared, notLambda,
   numberOfArgs, typesOfArgs, nonIterable) where
  import AbsStarsepLang
  import System.IO
  import Data.Char
  import System.Exit

  escapeChar :: Char
  escapeChar = chr 27
  errorColor :: IO ()
  errorColor = putStr $ escapeChar : "[31;1m"
  warningColor :: IO ()
  warningColor = putStr $ escapeChar : "[33;1m"
  normalColor :: String
  normalColor = escapeChar : "[0m"
  typeColor :: String
  typeColor = escapeChar : "[34;1m"

  typeString :: Type -> String
  typeString t =
    typeColor ++ show t ++ normalColor

  typeOfString :: Type -> String
  typeOfString t =
    " (typeof = " ++ typeString t ++ ") "

  printError :: String -> IO ()
  printError msg = do
    errorColor
    putStr "Error: "
    putStr normalColor
    putStrLn msg
    exitFailure

  printWarning :: String -> IO ()
  printWarning msg = do
    warningColor
    putStr "Warning: "
    putStr normalColor
    putStrLn msg

  parsing :: String -> IO ()
  parsing msg = do
    printError $ "parsing failed, " ++ msg

  typecheck :: String -> IO ()
  typecheck msg = printError $ "typecheck failed, " ++ msg

  typecheckWarn :: String -> IO ()
  typecheckWarn msg = printWarning $ "typecheck: " ++ msg

  multipleFnDef :: Ident -> IO ()
  multipleFnDef (Ident name) = typecheck $ "multiple definitions of function " ++ name

  noMain :: IO ()
  noMain = typecheck "there is no main function"

  badMain :: IO ()
  badMain = typecheck $ "main function has bad type, it " ++
                        "should be " ++ typeString Void ++ " without arguments"

  vRetNoVoid :: Type -> IO ()
  vRetNoVoid t = typecheck $ "return without value in function returning " ++ show t

  retVoid :: Expr -> IO ()
  retVoid e = typecheck $ "returning " ++ show e ++ " in void function"

  badRetType :: Expr -> Type -> Type -> IO ()
  badRetType e t rt = typecheck $ "returning " ++ show e ++
    typeOfString t ++ "in function returning " ++ show rt

  expectedExpression :: Expr -> Type -> Type -> IO ()
  expectedExpression e t expected = typecheck $ "expected expression of type " ++
    show expected ++ " got " ++ show e ++ typeOfString t

  shadowTopDef :: Ident -> IO ()
  shadowTopDef (Ident name) = typecheck $ "shadowing function " ++ name

  shadowVariable :: Ident -> IO ()
  shadowVariable (Ident name) = typecheckWarn $ "shadowing variable " ++ name

  variableUndeclared :: Ident -> IO ()
  variableUndeclared (Ident name) = typecheck $ "variable " ++ name
    ++ " is undeclared"

  noInit :: Ident -> Bool -> IO ()
  noInit (Ident name) isConst =
    typecheck $ "declaring " ++
      if isConst then "constant" else "variable with auto type" ++
      " " ++ name ++ " without init"

  changingConst :: Ident -> IO ()
  changingConst (Ident name) = typecheck $ "you cannot change constant " ++ name

  diffTypesBinOp :: Type -> Type -> IO ()
  diffTypesBinOp t1 t2 = typecheck $ "binary operation on different types: " ++
    show t1 ++ " and " ++ show t2

  sameArgNames :: Ident -> IO ()
  sameArgNames (Ident arg) = typecheck $ "duplicate argument name " ++ arg

  nonNumeric :: Expr -> Type -> IO ()
  nonNumeric expr t =
    typecheck $ show expr ++ " is not numeric" ++ typeOfString t

  nonBoolean :: Expr -> IO ()
  nonBoolean expr = typecheck $ show expr ++ " is not boolean"

  nonIterable :: Expr -> Type -> IO ()
  nonIterable expr t =
    typecheck $ show expr ++ " is not iterable" ++ typeOfString t

  functionUndeclared :: Ident -> IO ()
  functionUndeclared (Ident name) = typecheck $ "function " ++ name ++
    " is not declared in this scope"

  notLambda :: Ident -> IO ()
  notLambda (Ident name) = typecheck $ name ++ " is not a lambda nor function"

  numberOfArgs :: Ident -> Int -> Int -> IO ()
  numberOfArgs (Ident name) nArgs expected = typecheck $ "function " ++ name ++
    " expected " ++ show expected ++ " argument(s), " ++ show nArgs ++ " given"

  typesOfArgs :: Ident -> [Type] -> [Type] -> IO ()
  typesOfArgs (Ident name) argsTypes types = typecheck $ "function " ++
    name ++ " args types are " ++ show types ++
    ", trying to invoke function with args types " ++ show argsTypes
