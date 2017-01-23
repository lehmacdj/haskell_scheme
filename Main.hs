module Main where

import Parser
import LispError
import Eval

import System.Environment
import System.IO
import System.Console.Haskeline (getInputLine, runInputT, defaultSettings)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalString :: String -> IO String
evalString expr = pure $ unwrap $ trapError (show <$> (readExpr expr >>= evaluate))

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m (Maybe a) -> (a -> m ()) -> m ()
until_ guard prompt action = do
    (Just result) <- prompt
    if guard result
       then return ()
       else action result >> until_ guard prompt action

runRepl :: IO ()
runRepl = until_
    (=="quit")
    (runInputT defaultSettings $ getInputLine "Lisp>>> ")
    evalAndPrint

usage :: String
usage = "Usage: hscheme [expr]\n"
     ++ "   without expr loads an interactive Repl\n"
     ++ "   with expr evaluates expr and prints the result\n"

main :: IO ()
main = do
     args <- getArgs
     case length args of
       0 -> runRepl
       1 -> evalAndPrint $ args !! 0
       otherwise -> putStrLn usage
