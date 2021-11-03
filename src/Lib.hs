module Lib where

import Data.Maybe
import Text.Read
import Text.Printf
import Data.Char

cleanSpaces :: String -> String
cleanSpaces [] = []
cleanSpaces (' ':xs) = cleanSpaces xs
cleanSpaces ('\r':xs) = cleanSpaces xs
cleanSpaces ('\n':xs) = cleanSpaces xs
cleanSpaces ('\t':xs) = cleanSpaces xs
cleanSpaces (x:xs) = x:cleanSpaces xs

