module Main where

import Lib

import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import System.Environment

data LispValue 
    = Atom String
    | List [LispValue]
    | DottedList [LispValue] LispValue
    | Number Integer
    | Character Char
    | String String
    | Boolean Bool
    deriving (Show)

parseAtom :: Parser LispValue
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ case atom of
        "#t" -> Boolean True
        "#f" -> Boolean False
        _    -> Atom atom

parseNumber :: Parser LispValue
parseNumber = Number . read <$> many1 digit 

parseEscape :: Parser Char
parseEscape = foldl1 (<|>) $ char <$> "\"\\\a\t\r\n"

parseCharacter :: Parser LispValue
parseCharacter = do
    char '\''
    c <- alphaNum
    char '\''
    return $ Character c

parseString :: Parser LispValue
parseString = do
    char '"'
    body <- many (noneOf "\"" <|> parseEscape)
    char '"'
    return $ String body

parseExpression :: Parser LispValue
parseExpression = 
    parseAtom      <|>
    parseString    <|>
    parseCharacter <|>
    parseNumber

main :: IO ()
main = do
    putStr "=> "
    input <- getLine
    if input == "exit" 
        then 
            putStrLn "Bye for now."
        else do
            putStrLn $ readExpression input
            main

readExpression :: String -> String
readExpression input =
    case parse parseExpression "user_input" input of
        Left error -> "No Match: " ++ show error
        Right value -> "Found Value: " ++ show value

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space