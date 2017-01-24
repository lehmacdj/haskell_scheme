module LispError
( LispError(..)
, ThrowsError
, IOThrowsError
, runIOThrows
, liftThrows
, unwrap
, trapError
) where

import LispVal

import Control.Monad.Except

import Text.ParserCombinators.Parsec

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

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = action `catchError` (pure . show)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = pure val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= pure . unwrap

unwrap :: ThrowsError a -> a
unwrap (Right val) = val
