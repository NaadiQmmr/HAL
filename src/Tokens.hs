module Tokens where

import ParserLib
import Data.IORef

type Env = IORef [(String, IORef Token)]
data Token = Number Integer |
    Bool Bool |
    String String |
    Atom String |
    List [Token] |
    Primitive ([Token] -> Either ParserError Token) |
    Lambda { _params :: [Token], _args :: Maybe Token,
        _body :: [Token], _env :: Env }

instance Show Token where
    show (Bool True)            = "#t"
    show (Bool False)           = "#f"
    show (Number i)             = show i
    show (Atom s)               = ":" ++ s                  -- Like in Elixir
    show (String s)             = s
    show (List x)               = "(" ++ unlist x ++ ")"
    show (Primitive _)          = "<primitive>"
    show Lambda {_params = p}   = "<lambda (" ++ unlist p ++ ")."

unlist :: [Token] -> String
unlist = unwords . map show


