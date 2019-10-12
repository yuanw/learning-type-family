{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Add where

class Add a b where

  type SumTy a b

  add :: a -> b -> SumTy a b

instance Add Integer Double where

  type SumTy Integer Double = Double

  add x y = fromIntegral x + y

instance Add Integer Float where

  type SumTy Integer Float = Float

  add x y = fromIntegral x + y

x = 10 :: Float

y = 3 :: Integer

z = add y x
