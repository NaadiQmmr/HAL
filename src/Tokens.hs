module Tokens where

data Token = Number Integer |
    Bool Bool |
    String String |
    Atom String |
    List [Token]

instance Show Token where
    show (Bool True)            = "#t"
    show (Bool False)           = "#f"
    show (Number i)             = show i
    show (Atom s)               = ":" ++ s                  -- Like in Elixir
    show (String s)             = s
    show (List x)               = "(" ++ unlist x ++ ")"

unlist :: [Token] -> String
unlist = unwords . map show


