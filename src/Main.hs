module Main (main) where

import ParStarsepLang
import AbsStarsepLang
import ErrM
import qualified Errors
import Typecheck
import Control.Monad.Trans
import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), hGetContents, stdin)

main :: IO ()
main = do
  args <- getArgs
  file <- if null args then return stdin else openFile (head args) ReadMode
  code <- hGetContents file
  result <- interpreter code
  putStrLn result

interpret :: Program -> IO ()
interpret _ = return ()

interpreter :: String -> IO String
interpreter code = do
  prog <- case pProgram (myLexer code) of
    Ok p -> return p
    Bad msg -> do
      liftIO $ Errors.parsing msg
      return $ Program []
  typecheck prog
  interpret prog
  return "" -- $ show prog
