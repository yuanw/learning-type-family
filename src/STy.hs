{-# LANGUAGE GADTs #-}

module STy where

newtype Wrap a = Wrap a deriving (Show)

data STy ty where
    SInt :: STy Int
    SBool :: STy Bool
    SMaybe :: STy a -> STy (Maybe a)
    SWrap :: STy a -> STy (Wrap a)
    SList :: [STy a] -> STy [a]
    SApply :: STy a -> STy b -> STy (a -> b)

zero :: STy ty -> ty
zero SInt = 0
zero SBool = False
zero (SMaybe _) = Nothing
zero (SWrap a) = Wrap (zero a)
zero (SList _) = []
zero (SApply arg result) = const (zero result)
