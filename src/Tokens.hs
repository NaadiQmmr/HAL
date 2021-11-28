module Tokens where

import ParserLib
import CombinedParsers

import Control.Monad
import Control.Applicative
import Data.Char
import Data.IORef
import Data.Ratio
import Control.Monad.Except

type Env = IORef [(String, IORef Token)]
type IORuntimeError = ExceptT RuntimeError IO

type Run a = Either RuntimeError a
newtype Func = Func { function :: [Token] -> Run Token }

data RuntimeError = NumArgs Integer [Token]
               | TypeMismatch String Token
               | BadSpecialForm Token
               | NotFunction String
               | UnboundVar String
               | Default String

instance Show RuntimeError where
    show e = "*** RUNTIME ERROR : " ++ case e of
        Default s         -> s
        UnboundVar s      -> "Var " ++ s ++ " is not bound."
        NotFunction s     -> "Not a function: " ++ show s ++ "."
        BadSpecialForm t  -> "Bad special form: " ++ show t ++ "."
        NumArgs i tks     -> "Wrong number of arguments. Expected "
                    ++ show i ++ ", got " ++ show (length tks) ++ "."
        TypeMismatch s t  -> "Wrong Type. Expected " ++ s ++ ", got " ++
                    show t ++ "."

data Token = Number Integer |
    Bool Bool |
    String String |
    Atom String |
    List [Token] |
    Rat Rational | -- i miss Paris, you know ...
    ImproperList [Token] Token |
    Primitive String Func |
    Lambda Env Func |
    Nil

instance Show Token where
    show (Bool True)            = "#t"
    show (Bool False)           = "#f"
    show (Number i)             = show i
    show (Atom s)               = s
    show (String s)             = "\"" ++ s ++ "\""
    show (List x)               = "(" ++ unlist x ++ ")"
    show (Primitive name _)     = "<function " ++ name ++ ">"
    show (Lambda _ _)           = "<lambda>"
    show (ImproperList xs x)    = "(" ++ show x ++ " . " ++ unlist xs ++ ")"
    show Nil                    = "Nil"
    show (Rat r)                = show r

unlist :: [Token] -> String
unlist = unwords . map show

token :: Parser a -> Parser a
token p = between (with "(") p (with ")")

ratio :: Parser Token
ratio = do
    x <- many digit
    has '/'
    y <- many digit
    return $ Rat ((read x) % (read y))

string :: Parser Token
string = do
    has '"'
    s <- many $ escape <|> hasNoneOf "\""
    has '"'
    return $ String s
    where escape = has '\\' <* hasOneOf "\\\""

symbol :: Parser Char
symbol = hasOneOf "!$%&*+-./:<=>?@^_~"

atom :: Parser Token
atom = do
    x   <- letter <|> symbol
    xs  <- many $ digit <|> symbol <|> letter

    let together = x:xs
    return $ case together of
        "#t"    -> Bool True
        "#f"    -> Bool False
        _       -> Atom together

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
expr = atom <|> ratio <|> number <|> string <|> quote <|> do
    has '('
    parsed <- list <|> improperList
    has ')'
    return parsed
