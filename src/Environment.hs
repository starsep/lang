module Environment where
  import qualified Data.Map as Map
  import AbsStarsepLang
  import Control.Monad.RWS
  -- data Variable = V {
  --   isConst :: Bool,
  --   varType :: Type
  -- }
  -- type VarEnvironment = Map.Map String Variable
  -- type FnEnviroment = (String)
  -- type Environment = (VarEnvironment, FnEnviroment)
  type TypedFnDefs = Map.Map Ident Type
  type TCEnv = (TypedFnDefs, Type)
  type TCState = Map.Map Ident (Bool, Type)
  type TypecheckMonad = RWST TCEnv () TCState IO
