module Main where

import Oracle (OracleEnv(..), oracle, runOracle)

main :: IO ()
main = do
  let env = OracleEnv "YELLOW SUBMARINE" "AAAAAA" "BBBBBB"
  putStrLn $ runOracle (oracle "Not yet") env