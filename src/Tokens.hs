module Tokens where

import ParserLib
import CombinedParsers

import Data.IORef
import Control.Monad
import Control.Applicative
import Data.Char
import GHC.Real (Real)

type Env = IORef [(String, IORef Token)]
data Token = Number Integer |
    Bool Bool |
    String String |
    Atom String |
    List [Token] |
    ImproperList [Token] Token |
    Primitive { _name :: String, _f :: [Token] -> Either ParserError Token } |
    Lambda { _params :: [Token], _args :: Maybe Token,
        _body :: [Token], _env :: Env }

instance Show Token where
    show (Bool True)            = "#t"
    show (Bool False)           = "#f"
    show (Number i)             = show i
    show (Atom s)               = s
    show (String s)             = s
    show (List x)               = "(" ++ unlist x ++ ")"
    show (Primitive name _)          = "<primitive> (" ++ name ++ ")"
    show Lambda {_params = p}   = "<lambda (" ++ unlist p ++ ")."
    show (ImproperList xs x)    = "(" ++ show x ++ ":" ++ unlist xs ++ ")"


instance Eq Token where
    Bool a == Bool b            = a == b
    String a == String b        = a == b
    Atom a == Atom b            = a == b
    Number a == Number b        = a == b
    List a == List b            = a == b
    Primitive n _ == Primitive n' _ = n == n'
    Lambda p a b e == Lambda p' a' b' e' = p == p' && a == a' && b == b' && e == e'
    -- Lambda p a b e == Primitive n f
    _ == _ = False -- any other type comparison is False



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
list = List <$> sep expr spaces

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
