{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module Primitives
( primitives
) where


import LispVal
import Control.Monad.Except
import qualified Data.Map as M
import Text.Read (readMaybe)


-- unpackers

unpackNum :: LispVal -> Throws Integer
unpackNum (Number n) = pure n
unpackNum (String n) =
    maybe
        (throwError $ TypeMismatch "number" $ String n)
        pure
        $ readMaybe n
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> Throws Bool
unpackBool (Bool b) = pure b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackString :: LispVal -> Throws String
unpackString (String s) = pure s
unpackString (Number s) = pure $ show s
unpackString (Bool s)   = pure $ show s
unpackString notString  = throwError $ TypeMismatch "string" notString


-- list operations

car :: [LispVal] -> Eval LispVal
car [List (x : _)]         = pure x
car [DottedList (x : _) _] = pure x
car [badArg]               = throwError $ TypeMismatch "pair" badArg
car badArgs                = throwError $ NumArgs 1 badArgs

cdr :: [LispVal] -> Eval LispVal
cdr [List (_ : xs)]         = pure $ List xs
cdr [DottedList [_] x]      = pure x
cdr [DottedList (_ : xs) x] = pure $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgs                 = throwError $ NumArgs 1 badArgs

cons :: [LispVal] -> Eval LispVal
cons [x, List []] = pure $ List [x]
cons [x, List xs] = pure $ List $ x : xs
cons [x, DottedList xs xf] = pure $ DottedList (x : xs) xf
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgs = throwError $ NumArgs 2 badArgs


-- equality operations

-- precise equality, e.g. 1 == "1" is #f
eqv :: LispVal -> LispVal -> Bool
eqv (Bool x) (Bool y) = x == y
eqv (Number x) (Number y) = x == y
eqv (String x) (String y) = x == y
eqv (Atom x) (Atom y) = x == y
eqv (DottedList xs x) (DottedList ys y) =
    eqv (List $ xs ++ [x]) (List $ ys ++ [y])
eqv (List x) (List y) =
    (length x == length y) && (and $ zipWith eqv x y)
eqv _ _ = False

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> Throws a)
-- tries using an unpacker to compare two values
unpackEquals :: LispVal -> LispVal -> Unpacker -> Throws Bool
unpackEquals x y (AnyUnpacker unpacker) =
    do x' <- unpacker x
       y' <- unpacker y
       pure $ x' == y'
    `catchError` (const $ pure False)

-- rough equality, e.g. 1 == "1" is #t
equal :: LispVal -> LispVal -> Bool
equal (DottedList xs x) (DottedList ys y) = equal (List $ xs ++ [x]) (List $ ys ++ [y])
equal (List x) (List y) = (length x == length y) && (and $ zipWith equal x y)
equal x y =
    let primEq = or $ either (const False) id <$> map (unpackEquals x y) unpackers
        eqvEq = eqv x y
     in primEq || eqvEq
        where unpackers = [ AnyUnpacker unpackNum
                          , AnyUnpacker unpackString
                          , AnyUnpacker unpackBool ]


-- utility functions

liftEither :: Throws a -> Eval a
liftEither (Left err) = throwError err
liftEither (Right val) = pure val

binop :: (LispVal -> Throws a) -> (b -> LispVal) ->
    (a -> a -> b) -> [LispVal] -> Eval LispVal
binop from to op [from -> x, from -> y] = liftEither $ to <$> (op <$> x <*> y)
binop _ _ _ args = throwError $ NumArgs 2 args

multiop :: (LispVal -> Throws a) -> (a -> LispVal) ->
    (a -> a -> a) -> [LispVal] -> Eval LispVal
multiop _ _ _ [] = throwError $ NumArgs 2 []
multiop _ _ _ [arg] = throwError $ NumArgs 2 [arg]
multiop from to op args = liftEither $ to . foldl1 op <$> mapM from args

multiopNN = multiop unpackNum Number
multiopBB = multiop unpackBool Bool
binopNB = binop unpackNum Bool
binopSB = binop unpackString Bool
binopLB = binop pure Bool


-- the primitive functions for Lisp
primitives :: M.Map String ([LispVal] -> Eval LispVal)
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
    , ("eq?", binopLB eqv)
    , ("eqv?", binopLB eqv)
    , ("equal?", binopLB equal)
    ]
