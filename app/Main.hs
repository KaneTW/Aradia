module Main where

import Aradia
import Aradia.Types

import System.Environment

main :: IO ()
main = do
  token <- getEnv "ARADIA_TOKEN"
  runAradia $ AradiaConfig token ">"
