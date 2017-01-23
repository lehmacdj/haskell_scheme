module LispVal
( LispVal(..)
, showValList
) where

data LispVal = Atom String
             | Number Integer
             | String String
             | Bool Bool
             | List [LispVal]
             | DottedList [LispVal] LispVal

instance Show LispVal where
    show = showVal

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
