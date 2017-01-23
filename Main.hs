module Main where

import Parser
import LispError
import Eval
import System.Environment

main :: IO ()
main = do
     args <- getArgs
     let value = fmap show $ readExpr (args !! 0) >>= evaluate
     putStrLn $ unwrap $ trapError value
