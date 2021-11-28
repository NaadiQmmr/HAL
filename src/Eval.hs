module Eval where

import Tokens
import ParserLib
import Env
import Primitives

import Data.Either
import Data.Functor
import Control.Monad.State
import Control.Monad.Except

-- Pattern matching with val@SomeValue because we want to keep the type.
eval :: Env -> Token -> IORuntimeError Token
eval _ val@(Rat _) = return val
eval _ val@(List []) = return Nil
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval e val@(Atom name) = get' e name
eval _ val@Nil = return val
eval _ val@(List [_]) = return val
eval _ (List [Atom "quote", val]) = return val
eval e x = evalFuncs e x

evalFuncs :: Env -> Token -> IORuntimeError Token
evalFuncs e val@(List (Atom "cond":_)) = conditionnal e val
evalFuncs e val@(List (Atom "if":_)) = ifSpecialCase e val
evalFuncs e (List [Atom "set!", Atom v, f]) = eval e f >>= set e v
evalFuncs e (List [Atom "define", Atom v, f]) = eval e f >>= define e v
evalFuncs e (List (Atom f:a)) = mapM (eval e) a >>= lift' . apply f
evalFuncs _ err = lift' $ runError $ BadSpecialForm err

apply :: String -> [Token] -> Run Token
apply func args = maybe (runError $ NotFunction func)
                        ($ args)
                        (lookup func prims)

{-# ANN module "HLint: ignore Use lambda-case" #-}
ifSpecialCase :: Env -> Token -> IORuntimeError Token
ifSpecialCase e (List [Atom "if", pred, then', else']) =
    eval e pred >>= \res -> case res of
        Bool False  -> eval e then'
        _           -> eval e else'
ifSpecialCase e (List [Atom "if", pred, then']) =
    eval e pred >>= \res -> case res of
        Bool False  -> lift' $ runError
                        $ Default "Unexpected branching."
        _           -> eval e then'
ifSpecialCase _ _ = lift' $ runError $ Default "Expected predicate."

{-# ANN module "HLint: ignore Use lambda-case" #-}
conditionnal :: Env -> Token -> IORuntimeError Token
conditionnal e (List (Atom "cond":body)) = case body of
    []                              -> return $ List []
    (List [Atom "else", value]:_)   -> eval e value
    (List [pred, x]:xs)         -> eval e pred >>= \res -> caseof e res x xs
        where caseof env res x xs = case res of
                        List []     -> eval e $ List $ Atom "cond":xs
                        Bool False  -> eval e $ List $ Atom "cond":xs
                        _           -> eval e x
conditionnal _ _ = lift' $ runError
    $ Default "Entered condition without predicate."

