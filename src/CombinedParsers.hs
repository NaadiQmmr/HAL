module CombinedParsers where

import ParserLib
import Control.Applicative
import Data.Char
import Control.Monad

runParser :: Parser a -> String -> Maybe a
runParser p s = case parse p s of
    (Value _ a) -> Just a
    _           -> Nothing

hasOneOf :: String -> Parser Char
hasOneOf x = statisfy $ flip elem x

digit :: Parser Char
digit = statisfy isDigit

char :: Parser Char
char = Parser p
    where p []      = Error EOF
          p (x:xs)  = Value xs x

has :: Char -> Parser Char
has c = statisfy (c==)

statisfy :: (Char -> Bool) -> Parser Char
statisfy p = char >>= \c -> if p c then pure c else empty

hasString :: String -> Parser String
hasString = traverse has

between :: Parser o -> Parser a -> Parser c -> Parser a
between open p close = open >> (p >>= \res -> close >> return res)

select :: String -> Parser String
select s = selected $ hasString s
    where selected p = p >>= \a -> spaces >> return a
          spaces = many $ hasOneOf " \t\r\n"

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chain` op = p >>= \x -> rest x
    where rest x = (op >>= \f -> p >>= \y -> rest $ f x y) <|> return x

