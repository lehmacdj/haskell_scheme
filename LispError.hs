module LispError
( LispError(..)
, ThrowsError
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

trapError :: ThrowsError String -> ThrowsError String
trapError action = action `catchError` (pure . show)

unwrap :: ThrowsError a -> a
unwrap (Right val) = val
