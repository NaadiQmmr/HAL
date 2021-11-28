module Env where

import Data.IORef
import Control.Monad.Except

import Tokens
import ParserLib

emptyEnv :: IO Env
emptyEnv = newIORef []

parseError :: ParserError -> Run a
parseError e = Left $ Default $ show e

runError :: RuntimeError -> Run a
runError e = Left e

lift' :: Run a -> IORuntimeError a
lift' (Left a) = throwError a
lift' (Right a) = return a

isBound :: Env -> String -> IO Bool
isBound envRef x = readIORef envRef >>=
                   return . maybe False (const True) . lookup x

set :: Env -> String -> Token -> IORuntimeError Token
set envRef v val = do
    env <- liftIO $ readIORef envRef
    maybe   (lift' $ runError $ UnboundVar v)
            (liftIO . (flip writeIORef val))
            (lookup v env)
    return val

get' :: Env -> String -> IORuntimeError Token
get' envRef v = do
    env <- liftIO $ readIORef envRef
    maybe   (lift' $ runError $ UnboundVar v)
            (liftIO . readIORef)
            (lookup v env)

define :: Env -> String -> Token -> IORuntimeError Token
define envRef v val = do
    alreadyExists <- liftIO $ isBound envRef v
    if alreadyExists
    then set envRef v val >> return val
    else setnew v val envRef
    where setnew v val envRef = liftIO $ do
                            valRef  <- newIORef val
                            env     <- readIORef envRef
                            writeIORef envRef $ (v, valRef) : env
                            return val

bind :: Env -> [(String, Token)] -> IO Env
bind envRef arr = readIORef envRef >>= extend arr >>= newIORef
    where   extend arr env      = liftM (++ env) $ mapM addBind arr
            addBind (name, val) = newIORef val >>= \ref -> return (name, ref)


