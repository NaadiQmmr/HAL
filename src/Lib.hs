module Lib where

import Data.Maybe
import Text.Read
import Text.Printf
import Data.Char

cleanSpaces :: String -> String
cleanSpaces [] = []
cleanSpaces (x:xs) = case x of
    ' '     -> cleanSpaces xs
    '\r'    -> cleanSpaces xs
    '\n'    -> cleanSpaces xs
    '\t'    -> cleanSpaces xs
    _       -> x:cleanSpaces xs
