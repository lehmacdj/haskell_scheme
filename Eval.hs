{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval
( evaluate
, Env
, nullEnv
) where

import LispVal
import LispError
import Primitives

import qualified Data.Map as M

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except

import Data.IORef
import Data.Functor.Identity

type Env = IORef (M.Map String (IORef LispVal))

nullEnv :: IO Env
nullEnv = newIORef $ M.fromList []

newtype Eval a = Eval { runEval :: ReaderT Env IOThrowsError a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError LispError
             , MonadReader Env
             , MonadIO
             )

liftEval :: ThrowsError a -> Eval a
liftEval (Left err) = throwError err
liftEval (Right val) = pure val

isBound :: String -> Eval Bool
isBound var = do
    env <- ask >>= liftIO . readIORef
    pure $ maybe False (const True) $ M.lookup var env

getVar :: String -> Eval LispVal
getVar var = do
    env <- ask >>= liftIO . readIORef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (M.lookup var env)

setVar :: String -> LispVal -> Eval ()
setVar var value = do
    env <- ask >>= liftIO . readIORef
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
    env <- ask >>= liftIO . readIORef
    let func' = M.lookup func primitives
    args' <- mapM eval args
    liftEval $ maybe
        (throwError $ NotFunction "Unrecognized primitive function args" func)
        ($ args')
        func'

eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evaluate :: Env -> LispVal -> IOThrowsError LispVal
evaluate env val = runReaderT (runEval (eval val)) env
