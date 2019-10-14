{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

--https://youtu.be/6snteFntvjM?t=2988
module HList where

data HList tys where
  Nil :: HList '[]
  (:>) :: h -> HList t -> HList (h ': t)

infixr 5 :>

data Elem list elt where
  EZ :: Elem (x ': xs) x
  ES :: Elem xs x -> Elem (y ': xs) x

get :: Elem tys ty -> HList tys -> ty
get EZ (x :> y) = x
get (ES elem) (x :> y) = get elem y
