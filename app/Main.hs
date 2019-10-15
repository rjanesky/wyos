module Main where

import Lib

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Applicative hiding ((<|>), many)

data LispValue = Atom String
    | List [LispValue]
    | DottedList [LispValue] LispValue
    | Number Integer
    | String String
    | Bool Bool
    deriving (Show)

parseExpr :: Parser LispValue
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber

parseString :: Parser LispValue
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseNumber :: Parser LispValue
parseNumber = Number . read <$> many1 digit

parseAtom :: Parser LispValue
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = 
    case parse parseExpr "user-input" input of
        Left e -> "No Match: " ++ show e
        Right v -> "Value Found: " ++ show v

main :: IO ()
main = putStr "wyos=> " 
    >> getLine 
    >>= putStrLn . readExpr