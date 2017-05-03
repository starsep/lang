module Typecheck where
  import AbsStarsepLang
  import ErrM
  import qualified Errors
  typeOf :: Expr -> Type
  typeOf expr = Int
  typecheck :: Program -> IO ()
  typecheck prog = do
    -- Errors.typecheck "msg"
    return ()
