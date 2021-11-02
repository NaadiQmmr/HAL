module ParserLib where

import Data.Functor
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> Parsed a }

data Parsed a = Error ParserError | Value String a deriving (Eq)

data ParserError = EOF | ExpectedEOF String |
    ForbiddenChar Char | ForbiddenString String |
    Empty
    deriving (Eq)

instance Functor Parsed where
    fmap f (Value x a)          = Value x (f a)
    fmap _ (Error e)            = Error e

instance Functor Parser where
    fmap f (Parser p)           = Parser (fmap f . p)

instance Applicative Parser where
    pure x                      = Parser (`Value` x)
    (<*>) p x                   = p >>= (x <&>)

instance Monad Parser where
    (>>=) (Parser p) f          = Parser (\a -> case p a of
                                        Value rest x    -> parse (f x) rest
                                        Error e         -> Error e)

instance Alternative Parser where
    empty                       = Parser (\p -> Error Empty)
    (<|>) a b                   = a ||| b

(|||) :: Parser a -> Parser a -> Parser a
p1 ||| p2 = Parser (\x -> let f (Error _) = parse p2 x
                              f r = r in f $ parse p1 x)
