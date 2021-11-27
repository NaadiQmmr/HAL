module Repl where

import Lib
import Eval
import Tokens

import System.IO

import Control.Monad.State
import System.Exit

{-# ANN module "HLint: ignore Use lambda-case" #-}
repl :: IO ()
repl = input >>= \todo -> case todo of
    "quit"      -> exitSuccess
    "exit"      -> exitSuccess
    "Exit"      -> exitSuccess
    "Quit"      -> exitSuccess
    x           -> putStrLn (evaluated x) >> repl
    where input = putStr " > " >> hFlush stdout >> getLine

