module Main where

import Lib

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

main :: IO ()
main = do
    putStr "=> "
    input <- getLine
    if input == "exit" 
        then 
            putStrLn "Bye for now."
        else do
            putStrLn $ readExpr input
            main

readExpr :: String -> String
readExpr input =
    case parse (spaces >> symbol) "user_input" input of
        Left error -> "No Match: " ++ show error
        Right value -> "Found Value"

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space