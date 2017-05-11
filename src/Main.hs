module Main (main) where

import ParStarsepLang
import AbsStarsepLang
import ErrM
import Interpreter (interpret)
import qualified Errors
import qualified Debug
import Typecheck
import Control.Monad
import Control.Monad.Trans
import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), hGetContents, stdin)

parseFlags :: IO (Bool, [String])
parseFlags = do
  args <- getArgs
  let debug = "--debug" `elem` args
  let nonFlags = filter (not . (\l -> null l || head l == '-')) args
  return (debug, nonFlags)

main :: IO ()
main = do
  (debug, args) <- parseFlags
  file <- if null args then return stdin else openFile (head args) ReadMode
  code <- hGetContents file
  when debug $ Debug.printCode code
  result <- interpreter code
  putStrLn result

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
