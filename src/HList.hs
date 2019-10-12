{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

--https://youtu.be/6snteFntvjM?t=2988
module HList where

data HList tys where
  Nil :: HList '[]
  (:>) :: h -> HList t -> HList (h ': t)

infixr 5 :>
