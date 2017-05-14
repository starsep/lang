module Debug (printCode) where

import System.IO (stderr, hPutStrLn)

printCode :: String -> IO ()
printCode code = hPutStrLn stderr code
