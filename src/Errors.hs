module Errors (parsing, typecheck) where
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
    return $ ProgramDecl []

  typecheck :: String -> IO ()
  typecheck msg = printError $ "typecheck failed, " ++ msg
