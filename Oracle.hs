module Oracle
    (
      mkOracleEnv
    , oracle
    , runOracle
) where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, ask, runReaderT)

type Key = String

data OracleEnv = OracleEnv {
      key :: Key
    , suffix :: String
}

mkOracleEnv :: OracleEnv
mkOracleEnv = OracleEnv "YELLOW SUBMARINE" "AAAAAA"

type Oracle a = ReaderT OracleEnv Identity a

oracle    :: String -> Oracle String
oracle bs = do
  env <- ask
  return $ "[encrypt(" ++ bs ++ suffix env ++ ", " ++ key env ++ ")]"

runOracle         :: Oracle a -> OracleEnv -> a
runOracle ocl env = runIdentity (runReaderT ocl env)
