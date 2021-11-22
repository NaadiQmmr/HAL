module Eval where

import Tokens
import ParserLib
import Data.Either
import Data.Functor
import Control.Monad.State
import Control.Monad.Except
import Primitives

run :: String -> Run Token
run x = case parse expr x of
    Error err        -> Left $ Default $ show err
    Value _ tokens   -> eval tokens

trap :: (MonadError a m, Show a) => m String -> m String
trap x = catchError x $ return . show

-- Pattern matching with val@SomeValue because we want to keep the type.
eval :: Token -> Run Token
eval val@(String _) = Right val
eval val@(Number _) = Right val
eval val@(Bool _) = Right val
eval (List [Atom "quote", val]) = Right val
eval val@(List [Atom "cond", a, b, c]) = conditionnal val
eval val@(List [Atom "if", a, b, c]) = conditionnal val
eval (List (Atom f : a)) = mapM eval a >>= apply f
eval err = Left $ BadSpecialForm err

{-# ANN module "HLint: ignore Use lambda-case" #-}
conditionnal :: Token -> Run Token
conditionnal (List [Atom "if", pred, then', else']) =
        eval pred >>= \r -> case r of
                Bool False      -> eval else'
                _               -> eval then'
conditionnal _ = Left $ Default "Entered conditionnal without if statement."

apply :: String -> [Token] -> Run Token
apply f a = maybe (Left (NotFunction f)) ($ a) $ lookup f prims

get :: Run Token -> Token
get (Right x) = x
get _ = Nil
