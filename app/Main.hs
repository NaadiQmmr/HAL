module Main where

import Lib
import Eval
import Tokens
import FileExec
import Repl

import System.IO
import System.Environment

isRepl :: [String] -> Bool
isRepl ["-i"]   = True
isRepl _        = False

main :: IO ()
main = getArgs >>= \args -> if isRepl args then
                            repl else readFiles args
