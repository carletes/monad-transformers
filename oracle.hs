module Main where

import System.Exit (exitWith, ExitCode(..))

import Oracle (mkOracleEnv, oracle, runOracle)

main :: IO ()
main = do
  let env = mkOracleEnv
      loop :: IO ()
      loop = do
         bs <- getLine
         if bs == ""
            then exitWith $ ExitSuccess
         else do
             putStrLn $ runOracle (oracle bs) env
             loop
  loop
