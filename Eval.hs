{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval
( eval
, runEvalWithEnv
) where


import LispVal
import Primitives

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M
import Data.IORef
import Data.Functor.Identity


-- get the exposed environment (e.g. after reading the IORef)
getEnv :: Eval (M.Map String (IORef LispVal))
getEnv = ask >>= liftIO . readIORef

isBound :: String -> Eval Bool
isBound var = do
    env <- getEnv
    pure $ maybe False (const True) $ M.lookup var env

getVar :: String -> Eval LispVal
getVar var = do
    env <- getEnv
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (M.lookup var env)

setVar :: String -> LispVal -> Eval ()
setVar var value = do
    env <- getEnv
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef value))
          (M.lookup var env)

defineVar :: String -> LispVal -> Eval ()
defineVar var value = do
    isDefined <- isBound var
    envRef <- ask
    if isDefined
        then setVar var value
        else liftIO $ do
            env <- readIORef envRef
            ref <- newIORef value
            writeIORef envRef (M.insert var ref env)

eval :: LispVal -> Eval LispVal
eval val@(String _) = pure val
eval val@(Number _) = pure val
eval val@(Bool _) = pure val
eval (Atom ident) = getVar ident
eval (List [Atom "quote", val]) = pure val
eval (List [Atom "if", g, t, e]) = do
    g' <- eval g
    case g' of
      Bool False -> eval e
      otherwise  -> eval t
eval (List [Atom "set!", Atom var, form]) = do
    value <- eval form
    setVar var value
    pure value
eval (List [Atom "define", Atom var, form]) = do
    value <- eval form
    defineVar var value
    pure value
eval (List (Atom func : args)) = do
    env <- getEnv
    let func' = M.lookup func primitives
    args' <- mapM eval args
    maybe
        (throwError $ NotFunction "Unrecognized primitive function args" func)
        ($ args')
        func'
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- create an IO action that is the result of evaluating the expression
runEvalWithEnv :: Show a => Eval a -> Env -> IO (Throws a)
runEvalWithEnv val env = runExceptT (runReaderT (runEval val) env)
