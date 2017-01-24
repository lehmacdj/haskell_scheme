{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal
( LispVal(..)
, LispError(..), Throws(..), liftThrows, unwrapThrows
, Env, nullEnv
, Eval(..)
) where


import Control.Monad.Except
import Control.Monad.Reader

import System.Console.Haskeline.MonadException

import Data.IORef
import qualified Data.Map as M

import Text.ParserCombinators.Parsec (ParseError)


data LispVal = Atom String
             | Number Integer
             | String String
             | Bool Bool
             | List [LispVal]
             | DottedList [LispVal] LispVal

instance Show LispVal where show = showVal

showValList :: [LispVal] -> String
showValList = unwords . map showVal

showVal :: LispVal -> String
showVal (Atom i) = i
showVal (Number v) = show v
showVal (String s) = show s
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List vs) = "(" ++ (showValList vs) ++ ")"
showVal (DottedList vs t) = "(" ++ (showValList vs) ++ " . " ++ showVal t ++ ")"


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
    "Expected " ++ show expected ++ " args; found values " ++ showValList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type Throws = Either LispError

liftThrows :: MonadError e m => Either e a -> m a
liftThrows (Right val) = pure val
liftThrows (Left err) = throwError err

unwrapThrows :: Throws a -> a
unwrapThrows (Right val) = val

type Env = IORef (M.Map String (IORef LispVal))

nullEnv :: IO Env
nullEnv = newIORef $ M.fromList []


newtype Eval a = Eval { runEval :: ReaderT Env (ExceptT LispError IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError LispError
             , MonadReader Env
             , MonadIO
             , MonadException
             )

-- This is needed in order to use InputT Eval for the Repl
instance MonadException m => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) ->
        let run' = RunIO (fmap ExceptT . run . runExceptT)
         in fmap runExceptT $ f run'
