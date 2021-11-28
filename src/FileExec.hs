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
handleExists _ = return $ Nothing

readFiles :: Env -> [String] -> IO ()
readFiles _ [] = exitSuccess
readFiles env ("-i":_) = repl env
readFiles env (x:xs) = do
    maybeContent <- safeRead x
    case maybeContent of
        Nothing -> exitWith $ ExitFailure 84
                                    -- instead of lines -> split by ()
        Just content -> execContent (lines content) env >>= \str ->
            if null xs then putStrLn str else readFiles env xs

execContent :: [String] -> Env -> IO String
execContent [] _ = return ""
execContent (x:xs) env = evaluated env x >>= \res->
    if null xs then return res else execContent xs env

