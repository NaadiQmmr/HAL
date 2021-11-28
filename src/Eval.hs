module Eval where

import Tokens
import ParserLib
import Env
import Primitives

import Data.Either
import Data.Functor
import Data.Maybe
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
eval _ (List [Atom "\'", val]) = return val
eval e x = evalFuncs e x

evalFuncs :: Env -> Token -> IORuntimeError Token
evalFuncs e val@(List (Atom "cond":_)) = conditionnal e val
evalFuncs e val@(List (Atom "if":_)) = conditionnal e val
evalFuncs e (List [Atom "set!", Atom v, f]) = eval e f >>= set e v
evalFuncs e (List [Atom "define", Atom v, f]) = eval e f >>= define e v
evalFuncs e (List (Atom "define":List (Atom name:args):body)) =
    getNormalFunction e args body >>= define e name
evalFuncs e token = evalFuncs_ e token

evalFuncs_ :: Env -> Token -> IORuntimeError Token
evalFuncs_ e (List (Atom "lambda":List a:body)) = getNormalFunction e a body
evalFuncs_ e (List (Atom "lambda":ImproperList args vargs:body)) =
    getVargs vargs e args body
evalFuncs_ e (List (Atom "lambda":vargs@(Atom _):body)) =
    getVargs vargs e [] body
evalFuncs_ e (List (f:a)) = eval e f >>= \f' -> mapM (eval e) a
                            >>= \a' -> apply f' a'
evalFuncs_ _ err = lift' $ runError $ BadSpecialForm err

getFunction :: Maybe String -> Env -> [Token] -> [Token] -> IORuntimeError Token
getFunction vargs e args body = return $ Lambda (map show args) vargs body e

getNormalFunction :: Env -> [Token] -> [Token] -> IORuntimeError Token
getNormalFunction = getFunction Nothing

getVargs :: Token -> Env -> [Token] -> [Token] -> IORuntimeError Token
getVargs = getFunction . Just . show

apply :: Token -> [Token] -> IORuntimeError Token
apply (Primitive f) args = lift' $ f args
apply (Lambda a v b e) args = if len a /= len args && isNothing v
                    then lift' $ runError $ NumArgs (len a) args
                    else liftIO (bind e $ zip a args) >>= bind' v >>= eval'
        where   len = toInteger . length
                remArgs = drop (length a) args
                eval' env = last <$> mapM (eval env) b
                bind' arg env = case arg of
                    Just name -> liftIO $ bind env [(name, List $ remArgs)]
                    _         -> return env
apply f a = lift' $ runError $ Default $ "could not apply " ++ show f ++ show a

{-# ANN module "HLint: ignore Use lambda-case" #-}
conditionnal :: Env -> Token -> IORuntimeError Token
conditionnal e (List [Atom _, pred, then', else']) =
    eval e pred >>= \res -> case res of
        Bool False  -> eval e else'
        _           -> eval e then'
conditionnal e (List [Atom _, pred, then']) =
    eval e pred >>= \res -> case res of
        Bool False  -> lift' $ runError
                        $ Default "Unexpected branching."
        _           -> eval e then'
conditionnal _ _ = lift' $ runError $ Default "Expected predicate."

