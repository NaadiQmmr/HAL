module Error where

newtype Error = Error { reason :: String }
instance Show Error where
    show e = "*** ERROR : " ++ reason e
