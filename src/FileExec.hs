module FileExec where

import System.Exit
import Control.Exception
import Data.Maybe
import Eval
import System.IO

safeRead :: String -> IO (Maybe String)
safeRead path = fmap Just (readFile path) `catch` handleExists

handleExists :: IOException -> IO (Maybe String)
handleExists _ = return $ Nothing

readFiles :: [String] -> IO ()
readFiles [] = exitSuccess
readFiles (x:xs) = do
    maybeContent <- safeRead x
    case maybeContent of
        Nothing -> exitWith $ ExitFailure 84
        Just content -> execContent (lines content) >> readFiles xs

execContent :: [String] -> IO ()
execContent [] = return ()
execContent (x:xs) = case run x of
            Right r  -> execContent xs
            Left err -> exitWith $ ExitFailure 84

