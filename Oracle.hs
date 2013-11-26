module Oracle
    (
      mkOracleEnv
    , oracle
    , runOracle
) where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import System.Random (newStdGen, randoms)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

data OracleEnv = OracleEnv {
      key :: B.ByteString
    , suffix :: B.ByteString
}

mkOracleEnv :: IO OracleEnv
mkOracleEnv = do
  gen <- newStdGen
  return $ OracleEnv (B.pack $ take 16 $ randoms gen) (C.pack "AAAAAA")

type Oracle a = ReaderT OracleEnv Identity a

oracle    :: String -> Oracle String
oracle bs = do
  env <- ask
  return $ "[encrypt(" ++ bs ++ C.unpack (suffix env) ++ ", " ++ C.unpack (key env) ++ ")]"

runOracle         :: Oracle a -> OracleEnv -> a
runOracle ocl env = runIdentity (runReaderT ocl env)
