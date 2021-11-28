module FileExec where

import System.Exit
import Control.Exception
import Data.Maybe
import System.IO

import Env
import Eval
import Tokens
import Repl

safeRead :: String -> IO (Maybe String)
safeRead path = fmap Just (readFile path) `catch` handleExists

handleExists :: IOException -> IO (Maybe String)
handleExists _ = exitWith $ ExitFailure 84

readFiles :: Env -> [String] -> IO ()
readFiles _ [] = exitSuccess
readFiles env ("-i":_) = repl env
readFiles env (x:xs) = do
    maybeContent <- safeRead x
    case maybeContent of
        Nothing -> exitWith $ ExitFailure 84
        Just content -> evaluated env content >>= \str ->
            if null xs then putStrLn str else readFiles env xs

