module Lib (cleanSpaces, splitExpressions) where

import Data.Maybe
import Text.Read
import Text.Printf
import Data.Char

splitExpressions :: String -> [String]
splitExpressions x = subSplit x [] [] 0

subSplit :: String -> String -> [String] -> Int -> [String]
subSplit [] _ arr 0 = arr
subSplit [] _ _ _ = []
subSplit ('(':next) str arr 0 = subSplit next (str ++ "(") arr 1
subSplit ('(':next) str arr nb = subSplit next (str ++ "(") arr $ nb + 1
subSplit (')':next) str arr 1 = subSplit next [] (arr ++ [str ++ ")"]) 0
subSplit (')':next) str arr nb = subSplit next (str ++ ")") arr $ nb - 1
subSplit (x:next) str arr nb = subSplit next (str ++ [x]) arr nb

cleanSpaces :: String -> String
cleanSpaces [] = []
cleanSpaces (' ':xs) = cleanSpaces xs
cleanSpaces ('\r':xs) = cleanSpaces xs
cleanSpaces ('\n':xs) = cleanSpaces xs
cleanSpaces ('\t':xs) = cleanSpaces xs
cleanSpaces (x:xs) = x:cleanSpaces xs

