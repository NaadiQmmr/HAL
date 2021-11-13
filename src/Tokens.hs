module Tokens where

import ParserLib
import CombinedParsers

import Data.IORef
import Control.Monad
import Control.Applicative
import Data.Char

type Env = IORef [(String, IORef Token)]
data Token = Number Integer |
    Bool Bool |
    String String |
    Atom String |
    List [Token] |
    ImproperList [Token] Token |
    Primitive ([Token] -> Either ParserError Token) |
    Lambda { _params :: [Token], _args :: Maybe Token,
        _body :: [Token], _env :: Env }

instance Show Token where
    show (Bool True)            = "#t"
    show (Bool False)           = "#f"
    show (Number i)             = show i
    show (Atom s)               = s
    show (String s)             = s
    show (List x)               = "'(" ++ unlist x ++ ")"
    show (Primitive _)          = "<primitive>"
    show Lambda {_params = x}   = "(" ++ unlist x ++ ")"
    show (ImproperList xs x)    = "(" ++ show x ++ ":" ++ unlist xs ++ ")"

unlist :: [Token] -> String
unlist = unwords . map show

token :: Parser a -> Parser a
token p = between (with "(") p (with ")")

string :: Parser Token
string = between (has '\"') (many char) (has '\"') >>= \x -> return $ String x

symbol :: Parser Char
symbol = hasOneOf "!$%&*+-./:<=>?@^_~"

atom :: Parser Token
atom = do
    x   <- letter <|> symbol
    xs  <- many $ digit <|> symbol <|> letter
    return $ case x:xs of
        "#t"    -> Bool True
        "#f"    -> Bool False
        _       -> Atom $ x:xs

quote :: Parser Token
quote = has '\'' >> expr >>= \x -> return $ List [Atom "quote", x]

unary :: Parser String
unary = do
    signs <- many $ hasOneOf "+-"
    let filtered = filter (=='-') signs
    return $ if even (length filtered) then "" else "-"

number :: Parser Token
number = do
    sign <- unary
    nbs  <- some digit
    return $ Number $ read $ sign ++ nbs

list :: Parser Token
list = liftM List $ sep expr spaces

improperList :: Parser Token
improperList = do
    head <- end expr spaces
    tail <- has '.' >> spaces >> expr
    return $ ImproperList head tail

expr :: Parser Token
expr = atom <|> string <|> number <|> quote <|> do
    has '('
    parsed <- list <|> improperList
    has ')'
    return parsed
