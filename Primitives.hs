{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module Primitives where

import LispVal
import LispError
import qualified Data.Map as M

import Control.Monad.Except
import Text.Read (readMaybe)

type Context = M.Map String ([LispVal] -> ThrowsError LispVal)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = pure n
unpackNum (String n) =
    maybe
        (throwError $ TypeMismatch "number" $ String n)
        pure
        $ readMaybe n
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = pure b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = pure s
unpackString (Number s) = pure $ show s
unpackString (Bool s)   = pure $ show s
unpackString notString  = throwError $ TypeMismatch "string" notString

-- head
car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]         = pure x
car [DottedList (x : _) _] = pure x
car [badArg]               = throwError $ TypeMismatch "pair" badArg
car badArgs                = throwError $ NumArgs 1 badArgs

-- tail
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = pure $ List xs
cdr [DottedList [_] x]      = pure x
cdr [DottedList (_ : xs) x] = pure $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgs                 = throwError $ NumArgs 1 badArgs

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = pure $ List [x]
cons [x, List xs] = pure $ List $ x : xs
cons [x, DottedList xs xf] = pure $ DottedList (x : xs) xf
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgs = throwError $ NumArgs 2 badArgs

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool x), (Bool y)]     = pure $ Bool $ x == y
eqv [(Number x), (Number y)] = pure $ Bool $ x == y
eqv [(String x), (String y)] = pure $ Bool $ x == y
eqv [(Atom x), (Atom y)]     = pure $ Bool $ x == y
eqv [(DottedList xs x), (DottedList ys y)] =
    eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List x), (List y)] =
    pure $ Bool $ (length x == length y) && (and $ zipWith eqvBool x y)
        where eqvBool x y = case eqv [x, y] of
                              Left err -> False
                              Right (Bool val) -> val
eqv [_, _] = pure $ Bool False
eqv badArgs = throwError $ NumArgs 2 badArgs

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker unpacker) =
    do x' <- unpacker x
       y' <- unpacker y
       pure $ x' == y'
    `catchError` (const $ pure False)

equal :: [LispVal] -> ThrowsError LispVal
equal [DottedList xs x, DottedList ys y] =
    equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [List x, List y] =
    pure $ Bool $ (length x == length y) && (and $ zipWith equalBool x y)
        where equalBool x y = case equal [x, y] of
                                Left err -> False
                                Right (Bool val) -> val
equal [x, y] = do
    primEq <- or <$> mapM (unpackEquals x y) unpackers
    (Bool eqvEq) <- eqv [x, y]
    pure $ Bool $ primEq || eqvEq
        where unpackers = [ AnyUnpacker unpackNum
                          , AnyUnpacker unpackString
                          , AnyUnpacker unpackBool ]
equal badArgs = throwError $ NumArgs 2 badArgs

binop :: (LispVal -> ThrowsError a) -> (b -> LispVal) ->
    (a -> a -> b) -> [LispVal] -> ThrowsError LispVal
binop from to op [from -> x, from -> y] = to <$> (op <$> x <*> y)
binop _ _ _ args = throwError $ NumArgs 2 args

multiop :: (LispVal -> ThrowsError a) -> (a -> LispVal) ->
    (a -> a -> a) -> [LispVal] -> ThrowsError LispVal
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
    , ("car", car)
    , ("cdr", cdr)
    , ("cons", cons)
    , ("eq?", eqv)
    , ("eqv?", eqv)
    , ("equal?", equal)
    ]
