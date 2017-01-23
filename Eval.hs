{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval
( evaluate
) where

import LispVal
import LispError

import qualified Data.Map as M
import Text.Read (readMaybe)

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Data.Functor.Identity

type Context = M.Map String ([LispVal] -> Eval LispVal)

newtype Eval a = Eval { unEval :: ExceptT LispError (Reader Context) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError LispError
             , MonadReader Context
             )


unpackNum :: LispVal -> Eval Integer
unpackNum (Number n) = pure n
unpackNum (String n) =
    maybe
        (throwError $ TypeMismatch "number" $ String n)
        pure
        $ readMaybe n
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> Eval Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackString :: LispVal -> Eval String
unpackString (String s) = return s
unpackString (Number s) = return $ show s
unpackString (Bool s)   = return $ show s
unpackString notString  = throwError $ TypeMismatch "string" notString

binop :: (LispVal -> Eval a) -> (b -> LispVal) ->
    (a -> a -> b) -> [LispVal] -> Eval LispVal
binop from to op [from -> x, from -> y] = to <$> (op <$> x <*> y)
binop _ _ _ args = throwError $ NumArgs 2 args

multiop :: (LispVal -> Eval a) -> (a -> LispVal) ->
    (a -> a -> a) -> [LispVal] -> Eval LispVal
multiop _ _ _ [] = throwError $ NumArgs 2 []
multiop _ _ _ [arg] = throwError $ NumArgs 2 [arg]
multiop from to op args = to . foldl1 op <$> mapM from args

multiopNN = multiop unpackNum Number
multiopBB = multiop unpackBool Bool
binopNB = binop unpackNum Bool
binopSB = binop unpackString Bool

primitives :: Context
primitives = M.fromList
    [ ("+", multiopNN (+))
    , ("-", multiopNN (-))
    , ("*", multiopNN (*))
    , ("/", multiopNN div)
    , ("mod", multiopNN mod)
    , ("quotient", multiopNN quot)
    , ("remainder", multiopNN rem)
    , ("=", binopNB (==))
    , ("<", binopNB (<))
    , (">", binopNB (>))
    , ("/=", binopNB (/=))
    , (">=", binopNB (>=))
    , ("<=", binopNB (<=))
    , ("&&", multiopBB (&&))
    , ("||", multiopBB (||))
    , ("string=?", binopSB (==))
    , ("string<?", binopSB (<))
    , ("string>?", binopSB (>))
    , ("string<=?", binopSB (<=))
    , ("string>=?", binopSB (>=))
    ]

eval :: LispVal -> Eval LispVal
eval val@(String _) = pure val
eval val@(Number _) = pure val
eval val@(Bool _) = pure val

eval (List [Atom "quote", val]) = pure val

eval (List [Atom "if", g, t, e]) = do
    g' <- eval g
    case g' of
      Bool False -> eval e
      otherwise  -> eval t

eval (List (Atom func : args)) = do
    env <- ask
    let func' = M.lookup func env
    args' <- mapM eval args
    maybe
        (throwError $ NotFunction "Unrecognized primitive function args" func)
        ($ args')
        func'

eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evaluate :: LispVal -> ThrowsError LispVal
evaluate val = runReader (runExceptT $ unEval (eval val)) primitives
