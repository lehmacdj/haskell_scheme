module Main where

import Parser
import LispError
import Eval

import Control.Monad

import System.Environment
import System.IO
import System.Console.Haskeline (getInputLine, runInputT, defaultSettings)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= evaluate env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m (Maybe a) -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    case mfilter (not . pred) result of
      Nothing ->  pure ()
      Just result' -> action result' >> until_ pred prompt action

runOnce :: String -> IO ()
runOnce expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_
    (=="quit")
    (runInputT defaultSettings $ getInputLine "Lisp>>> ")
    . evalAndPrint

usage :: String
usage = "Usage: hscheme [expr]\n"
     ++ "   without expr loads an interactive Repl\n"
     ++ "   with expr evaluates expr and prints the result\n"

main :: IO ()
main = do
     args <- getArgs
     case length args of
       0 -> runRepl
       1 -> runOnce $ args !! 0
       otherwise -> putStrLn usage
