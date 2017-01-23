{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval
( evaluate
) where

import LispVal
import LispError
import Primitives

import qualified Data.Map as M

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Data.Functor.Identity

newtype Eval a = Eval { unEval :: ReaderT Context ThrowsError a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError LispError
             , MonadReader Context
             )

liftEval :: ThrowsError a -> Eval a
liftEval (Left err) = throwError err
liftEval (Right val) = pure val

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
    liftEval $ maybe
        (throwError $ NotFunction "Unrecognized primitive function args" func)
        ($ args')
        func'

eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evaluate :: LispVal -> ThrowsError LispVal
evaluate val = runReaderT (unEval (eval val)) primitives
