module Environment where
  import qualified Data.Map as Map
  import AbsStarsepLang
  data Variable = V {
    isConst :: Bool,
    varType :: Type
  }
  type VarEnvironment = Map.Map String Variable
  type FnEnviroment = (String)
  type Environment = (VarEnvironment, FnEnviroment)