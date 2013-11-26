module Main where

import System.Exit (exitWith, ExitCode(..))

import Oracle (OracleEnv(..), oracle, runOracle)

main :: IO ()
main = do
  let env = OracleEnv "YELLOW SUBMARINE" "AAAAAA"
      loop :: IO ()
      loop = do
         bs <- getLine
         if bs == ""
            then exitWith $ ExitSuccess
         else do
             putStrLn $ runOracle (oracle bs) env
             loop
  loop
