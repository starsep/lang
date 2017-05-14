module Debug (printCode) where

import System.IO (stderr, hPutStrLn)

printCode :: String -> IO ()
printCode = hPutStrLn stderr
