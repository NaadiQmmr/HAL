module Main where

import Lib
import Eval
import Tokens
import FileExec
import Repl
import Env

import System.IO
import System.Environment
import FileExec
import Repl
import System.IO
import System.Environment

isRepl :: [String] -> Bool
isRepl ["-i"]   = True
isRepl _        = False

main :: IO ()
main = do
    env <- emptyEnv
    args <- getArgs
    if isRepl args then repl env else readFiles env args

