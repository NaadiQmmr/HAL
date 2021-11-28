module Repl where

import Lib
import Env
import Eval
import Tokens
import ParserLib
import CombinedParsers

import System.IO
import Control.Monad.State
import Control.Monad.Except
import System.Exit
import Data.IORef

{-# ANN module "HLint: ignore Use lambda-case" #-}
repl :: Env -> IO ()
repl env = input >>= \todo -> case todo of
    "quit"      -> exitSuccess
    "exit"      -> exitSuccess
    "Exit"      -> exitSuccess
    "Quit"      -> exitSuccess
    x           -> run env x >> repl env
    where input = putStr "> " >> hFlush stdout >> getLine

run :: Env -> String -> IO ()
run env x = evaluated env x >>= putStrLn

runSingle :: String -> IO String
runSingle x = do
    env     <- emptyEnv
    result  <- evaluated env x
    return result

throws :: IORuntimeError String -> IO String
throws x = runExceptT (trap x) >>= return . get''
    where   trap a          = catchError a (return . show)
            get'' (Right a) = a

evaluated :: Env -> String -> IO String
evaluated env x = throws $ liftM show $ (lift' $ run' x) >>= eval env
                where run' x = runExpr expr x

runExpr :: Parser a -> String -> Run a
runExpr p s = case parse p s of
    (Value _ a)     -> return a
    (Error e)       -> parseError e
