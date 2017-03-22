module Parser
( readExpr
) where


import LispVal
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
    pure $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    pure $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseProtoList :: Parser [LispVal]
parseProtoList = do
    _ <- char '('
    parseExpr `sepEndBy` spaces

parseListEnd :: Parser ([LispVal] -> LispVal)
parseListEnd = do
    _ <- char ')'
    pure $ \proto -> List proto

parseDottedListEnd :: Parser ([LispVal] -> LispVal)
parseDottedListEnd = do
    tl <- char '.' >> spaces >> parseExpr
    pure $ \proto -> DottedList proto tl

parseListVariant :: Parser LispVal
parseListVariant = do
    proto <- parseProtoList
    ender <- parseListEnd <|> parseDottedListEnd
    pure $ ender proto

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    pure $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
    parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> parseListVariant

readExpr :: String -> Throws LispVal
readExpr str = case parse parseExpr "lisp" str of
                 Left err -> throwError $ Parser err
                 Right val -> pure val
