module Errors
  (parsing, typecheck, multipleFnDef, noMain, badMain, vRetNoVoid, retVoid,
   badRetType) where
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
  normalColor :: IO ()
  normalColor = putStr $ escapeChar : "[0m"

  printError :: String -> IO ()
  printError msg = do
    errorColor
    putStr "Error: "
    normalColor
    putStrLn msg
    exitFailure

  printWarning :: String -> IO ()
  printWarning msg = do
    warningColor
    putStr "Warning: "
    normalColor
    putStrLn msg

  parsing :: String -> IO Program
  parsing msg = do
    printError $ "parsing failed, " ++ msg
    return $ Program []

  typecheck :: String -> IO ()
  typecheck msg = printError $ "typecheck failed, " ++ msg

  multipleFnDef :: Ident -> IO ()
  multipleFnDef (Ident name) = typecheck $ "multiple definitions of function " ++ name

  noMain :: IO ()
  noMain = typecheck "there is no main function"

  badMain :: IO ()
  badMain = typecheck $ "main function has bad type, it " ++
                        "should be void without arguments"

  vRetNoVoid :: Type -> IO ()
  vRetNoVoid t = typecheck $ "return without value in function returning " ++ show t

  retVoid :: Expr -> IO ()
  retVoid e = typecheck $ "returning " ++ show e ++ " in void function"

  badRetType :: Expr -> Type -> Type -> IO ()
  badRetType e t rt = typecheck $ "returning " ++ show e ++ " (typeof = " ++
    show t ++ ") in function returning " ++ show rt
