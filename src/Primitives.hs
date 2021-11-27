module Primitives where

import Data.Functor
import Tokens

nPrims :: [(String, [Token] -> Run Token)]
nPrims = [("+", numPrim (+)),
        ("-", numPrim (-)),
        ("*", numPrim (*)),
        ("/", numPrim div),
        ("mod", numPrim mod),
        ("quotient", numPrim quot),
        ("remainder", numPrim rem)]

bPrims :: [(String, [Token] -> Run Token)]
bPrims = [("=", nBoolPrim (==)),
        ("<", nBoolPrim (<)),
        (">", nBoolPrim (>)),
        ("/=", nBoolPrim (/=)),
        ("<=", nBoolPrim (<=)),
        (">=", nBoolPrim (>=)),
        ("&&", bBoolPrim (&&)),
        ("||", bBoolPrim (||))]
        where   nBoolPrim = boolPrim unpackNb
                bBoolPrim = boolPrim unpackB

sPrims :: [(String, [Token] -> Run Token)]
sPrims = [("string=?", sBoolPrim (==)),
        ("string<?", sBoolPrim (<)),
        ("string>?", sBoolPrim (>)),
        ("string<=?", sBoolPrim (<=)),
        ("string>=?", sBoolPrim (>=))]
        where sBoolPrim = boolPrim unpackS

-- FIXME: equal? should test interpreted values
prims :: [(String, [Token] -> Run Token)]
prims = nPrims ++ bPrims ++ sPrims ++
        [("car", car), ("cdr", cdr), ("cons", cons), ("eq?", eqv),
        ("eqv?", eqv), ("equal?", eqv)]

boolPrim :: (Token -> Run a) -> (a -> a -> Bool) -> [Token] -> Run Token
boolPrim unpack op args = if length args /= 2
        then Left (NumArgs 2 args)
        else do first   <- unpack $ head args
                second  <- unpack $ args !! 1
                return $ Bool $ first `op` second

numPrim :: (Integer -> Integer -> Integer) -> [Token] -> Run Token
numPrim op x = if length x /= 2 then Left $ NumArgs 2 x
        else mapM unpackNb x <&> (Number . foldl1 op)

unpackNb :: Token -> Run Integer
unpackNb (Number n) = return n
unpackNb (List [n]) = unpackNb n
unpackNb (String n) = let tuple = reads n :: [(Integer, String)] in
                    if null tuple then Left $ TypeMismatch "number" $ String n
                    else return $ fst $ head tuple
unpackNb x = Left $ TypeMismatch "number" x

unpackS :: Token -> Run String
unpackS (String s) = return s
unpackS (Number s) = return $ show s
unpackS (Bool s)   = return $ show s
unpackS x           = Left $ TypeMismatch "string" x

unpackB :: Token -> Run Bool
unpackB (Bool b) = return b
unpackB b        = Left $ TypeMismatch "boolean" b

car :: [Token] -> Run Token
car [List (x:_)]                = return x
car [ImproperList xs x]         = return x
car [any]                       = Left $ TypeMismatch "pair" any
car x                           = Left $ NumArgs 1 x

cdr :: [Token] -> Run Token
cdr [List (_:xs)]               = if null xs then return (Nil)
                                  else return $ List xs
cdr [ImproperList [_] x]        = return x
cdr [ImproperList (_:xs) x]     = return $ ImproperList xs x
cdr (x:_)                       = Left $ TypeMismatch "pair" x
cdr []                          = Left $ TypeMismatch "pair" Nil

cons :: [Token] -> Run Token
cons [x1, Nil]                  = return $ List [x1]
cons [x, List xs]               = return $ List $ x:xs
cons [x, ImproperList xs x']    = return $ ImproperList (x:xs) x'
cons [x1, x2]                   = return $ ImproperList [x1] x2
cons else'                      = Left $ NumArgs 2 else'

eqv :: [Token] -> Run Token
eqv [Bool a1, Bool a2]          = return $ Bool $ a1 == a2
eqv [Number a1, Number a2]      = return $ Bool $ a1 == a2
eqv [String a1, String a2]      = return $ Bool $ a1 == a2
eqv [Atom a1, Atom a2]          = return $ Bool $ a1 == a2
eqv [ImproperList xs x, ImproperList ys y] =
        eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List a1, List a2]          = return $ Bool $ (length a1 == length a2) &&
        all eqvPair (zip a1 a2)
eqv [_, _]                      = return $ Bool False
eqv x                           = Left $ NumArgs 2 x

eqvPair :: (Token, Token) -> Bool
eqvPair (x, y) = case eqv [x, y] of
                Left e          -> False
                Right (Bool v)  -> v
                _               -> False

