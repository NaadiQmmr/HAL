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

evaluated :: String -> String
evaluated x = case run x of
    Left x  -> show x
    Right x -> show x

-- Pattern matching with val@SomeValue because we want to keep the type.
eval :: Token -> Run Token
eval val@(List []) = return Nil
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval val@Nil = return val
eval val@(List [_]) = return val
eval (List [Atom "quote", val]) = return val
eval x = evalFuncs x

evalFuncs :: Token -> Run Token
evalFuncs val@(List (Atom "cond":_)) = conditionnal val
evalFuncs val@(List (Atom "if":_)) = ifSpecialCase val
evalFuncs (List (Atom f:a)) = mapM eval a >>= \args -> apply f args
evalFuncs err = Left $ BadSpecialForm err

apply :: String -> [Token] -> Run Token
apply f a = maybe (Left $ NotFunction f) ($ a) $ lookup f prims

{-# ANN module "HLint: ignore Use lambda-case" #-}
ifSpecialCase :: Token -> Run Token
ifSpecialCase (List [Atom "if", pred, then', else']) =
    eval pred >>= \res -> case res of
        Bool False  -> eval then'
        _           -> eval else'
ifSpecialCase (List [Atom "if", pred, then']) =
    eval pred >>= \res -> case res of
        Bool False  -> Left $ Default "Unexpected branching."
        _           -> eval then'
ifSpecialCase _ = Left $ Default "Expected predicate."

{-# ANN module "HLint: ignore Use lambda-case" #-}
conditionnal :: Token -> Run Token
conditionnal (List (Atom "cond":body)) = case body of
    []                              -> return $ List []
    (List [Atom "else", value]:_)   -> eval value
    (List [pred, x]:xs)         -> eval pred >>= \res -> caseof res x xs
        where caseof res x xs = case res of
                        List []     -> eval $ List $ Atom "cond":xs
                        Bool False  -> eval $ List $ Atom "cond":xs
                        _           -> eval x
conditionnal _ = Left $ Default "Entered conditionnal without if statement."

