{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib.NonEmptyList
       ( NonEmptyList(..)
       , singleton
       , cons
       , lookup
       )
       where

import           Data.Binary    (Binary (..))
import           GHC.Generics   (Generic)
import           Prelude.Compat hiding (head, lookup, tail)

data NonEmptyList a =
  NonEmptyList
  { head :: a
  , tail :: [a]
  }
  deriving (Show, Eq, Generic, Functor, Traversable, Foldable)
instance Binary a => Binary (NonEmptyList a)

singleton :: a -> NonEmptyList a
singleton x = NonEmptyList x []

cons :: a -> NonEmptyList a -> NonEmptyList a
cons x (NonEmptyList y ys) = NonEmptyList x (y:ys)

lookup :: Eq a => a -> NonEmptyList (a, b) -> Maybe b
lookup x (NonEmptyList (y,b) ys) = go x ((y, b):ys)
  where
    go _  [] = Nothing
    go x' ((y', b'):ys')
      | x' == y'  = Just b'
      | otherwise = go x' ys'
