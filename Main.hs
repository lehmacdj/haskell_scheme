module Main where

import LispVal
import Parser
import Eval

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (liftIO)

import System.Environment
import System.IO
import System.Console.Haskeline

-- convert an error into a string
trapError :: (MonadError e m, Show e) => m String -> m String
trapError = flip catchError (pure . show)

evalString :: String -> Eval LispVal
evalString expr = liftThrows (readExpr expr) >>= eval

evalAndPrint :: String -> Eval ()
evalAndPrint expr = trapError (show <$> evalString expr) >>= liftIO . putStrLn

until_ :: Monad m => (a -> Bool) -> m (Maybe a) -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    case mfilter (not . pred) result of
      Nothing -> pure ()
      Just result' -> action result' >> until_ pred prompt action

runOnce :: String -> IO ()
runOnce expr = fmap unwrapThrows $ nullEnv >>= runEvalWithEnv (evalAndPrint expr)

runRepl :: IO ()
runRepl = fmap unwrapThrows $ nullEnv >>=
    (runEvalWithEnv $
    runInputT defaultSettings $
    until_
        (==":q")
        (getInputLine "Lisp>>> ")
        (lift . evalAndPrint))

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
