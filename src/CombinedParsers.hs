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
hasOneOf x = satisfy $ flip elem x

digit :: Parser Char
digit = satisfy isDigit

char :: Parser Char
char = Parser p
    where p []      = Error EOF
          p (x:xs)  = Value xs x

has :: Char -> Parser Char
has c = satisfy (c==)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = char >>= \c -> if p c then pure c else empty

with :: String -> Parser String
with = traverse has

between :: Parser o -> Parser a -> Parser c -> Parser a
between open p close = open >> (p >>= \res -> close >> return res)

spaces :: Parser String
spaces = many $ hasOneOf " \t\r\n"

select :: String -> Parser String
select s = selected $ with s
    where selected p = p >>= \a -> spaces >> return a

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chain` op = p >>= \x -> rest x
    where rest x = (op >>= \f -> p >>= \y -> rest $ f x y) <|> return x

