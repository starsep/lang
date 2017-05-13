module Errors
  (parsing, typecheck, multipleFnDef, noMain, badMain, vRetNoVoid, retVoid,
   badRetType, expectedExpression, shadowTopDef, shadowVariable,
   variableUndeclared, noInit, changingConst, diffTypesBinOp,
   sameArgNames, nonNumeric, nonBoolean, functionUndeclared, notLambda,
   numberOfArgs, typesOfArgs, nonIterable, nonComparable, nonPrintable,
   listDiffJoin, nonListType, assert) where
  import AbsStarsepLang
  import PrintStarsepLang
  import Data.Char
  import Data.List
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
  exprColor :: String
  exprColor = escapeChar : "[35;1m"

  typeString :: Type -> String
  typeString t =
    typeColor ++ printTree t ++ normalColor

  typesString :: [Type] -> String
  typesString t = concat $ ("[" : intersperse "," (map typeString t)) ++ ["]"]

  exprString :: Expr -> String
  exprString e =
    exprColor ++ printTree e ++ normalColor

  numString :: Int -> String
  numString n = exprColor ++ show n ++ normalColor

  identString :: Ident -> String
  identString ident = exprString $ EVar ident

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
  parsing msg = printError $ "parsing failed, " ++ msg

  typecheck :: String -> IO ()
  typecheck msg = printError $ "typecheck failed, " ++ msg

  typecheckWarn :: String -> IO ()
  typecheckWarn msg = printWarning $ "typecheck: " ++ msg

  multipleFnDef :: Ident -> IO ()
  multipleFnDef ident =
    typecheck $ "multiple definitions of function " ++ identString ident

  noMain :: IO ()
  noMain = typecheck "there is no main function"

  badMain :: IO ()
  badMain = typecheck $ "main function has bad type, it " ++
                        "should be " ++ typeString Void ++ " without arguments"

  vRetNoVoid :: Type -> IO ()
  vRetNoVoid t =
    typecheck $ "return without value in function returning " ++ typeString t

  retVoid :: Expr -> IO ()
  retVoid e = typecheck $ "returning " ++ exprString e ++ " in void function"

  badRetType :: Expr -> Type -> Type -> IO ()
  badRetType e t rt = typecheck $ "returning " ++ exprString e ++
    typeOfString t ++ "in function returning " ++ typeString rt

  expectedExpression :: Expr -> Type -> Type -> IO ()
  expectedExpression e t expected = typecheck $ "expected expression of type " ++
    typeString expected ++ " got " ++ exprString e ++ typeOfString t

  shadowTopDef :: Ident -> IO ()
  shadowTopDef ident = typecheck $ "shadowing function " ++ identString ident

  shadowVariable :: Ident -> IO ()
  shadowVariable ident =
    typecheckWarn $ "shadowing variable " ++ identString ident

  variableUndeclared :: Ident -> IO ()
  variableUndeclared ident = typecheck $ "variable " ++ identString ident
    ++ " is undeclared"

  noInit :: Ident -> Bool -> IO ()
  noInit ident isConst =
    typecheck $ "declaring " ++
      if isConst then "constant" else "variable with auto type" ++
      " " ++ identString ident ++ " without init"

  changingConst :: Ident -> IO ()
  changingConst ident =
    typecheck $ "you cannot change constant " ++ identString ident

  diffTypesBinOp :: Type -> Type -> IO ()
  diffTypesBinOp t1 t2 = typecheck $ "binary operation on different types: " ++
    typeString t1 ++ " and " ++ typeString t2

  sameArgNames :: Ident -> IO ()
  sameArgNames ident =
    typecheck $ "duplicate argument name " ++ identString ident

  nonNumeric :: Expr -> Type -> IO ()
  nonNumeric expr t =
    typecheck $ exprString expr ++ " is not numeric" ++ typeOfString t

  nonBoolean :: Expr -> IO ()
  nonBoolean expr = typecheck $ exprString expr ++ " is not boolean"

  nonIterable :: Expr -> Type -> IO ()
  nonIterable expr t =
    typecheck $ exprString expr ++ " is not iterable" ++ typeOfString t

  functionUndeclared :: Ident -> IO ()
  functionUndeclared ident = typecheck $ "function " ++ identString ident ++
    " is not declared in this scope"

  notLambda :: Ident -> IO ()
  notLambda ident = typecheck $ identString ident ++ " is not a lambda nor function"

  numberOfArgs :: Ident -> Int -> Int -> IO ()
  numberOfArgs ident nArgs expected =
    typecheck $ "function " ++ identString ident ++ " expected " ++
    numString expected ++ " argument(s), " ++ numString nArgs ++ " given"

  typesOfArgs :: Ident -> [Type] -> [Type] -> IO ()
  typesOfArgs ident argsTypes types = typecheck $ "function " ++
    identString ident ++ " args types are " ++ typesString types ++
    ", trying to invoke function with args types " ++ typesString argsTypes

  nonComparable :: Expr -> Type -> IO ()
  nonComparable expr t = typecheck $ "expected iterable expression, got " ++
    exprString expr ++ " which is" ++ typeOfString t

  nonPrintable :: Expr -> Type -> IO ()
  nonPrintable expr t = typecheck $ exprString expr ++ " is not printable" ++
    typeOfString t

  listDiffJoin :: Expr -> Expr -> Type -> Type -> IO ()
  listDiffJoin a b ta tb =
    typecheck $ "trying to join lists of different types " ++ exprString a ++
    typeOfString ta ++ exprString b ++ typeOfString tb

  nonListType :: Expr -> Type -> IO ()
  nonListType e t = typecheck $ "expected list type, got " ++ exprString e ++
    typeOfString t

  assert :: Expr -> IO ()
  assert e = do
    errorColor
    putStr "Assert: "
    putStr normalColor
    putStrLn $ "expression " ++ exprString e ++ " failed"
    exitFailure
