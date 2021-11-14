module Eval where

import Tokens
import Data.Either
import Control.Exception.Base (runtimeError)

cons :: Token -> Token -> Token
cons a b = List[a, b]

car :: Token -> Either runtimeError Token
car (List []) = Right $ List []
car (List (x:_)) = Right x
car _ = Left $ error "***ERROR: argument of car should be a list."

cdr :: Token -> Either runTimeError Token
cdr (List []) = Right $ List []
cdr (List (_: xs)) = Right $ List xs
cdr _ = Left $ error "***ERROR: argument of cdr should be a list."