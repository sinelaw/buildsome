{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Lib.Trie where

import Lib.NonEmptyMap (NonEmptyMap)
import Data.Binary (Binary)
import GHC.Generics (Generic)

import Prelude.Compat

-- | A non-empty trie with values only at leaves, and multiple payloads ("descs") for each key
data Trie key keyDesc value tree
  = Branch (NonEmptyMap key (NonEmptyMap keyDesc tree))
  | Leaf value
  deriving (Generic, Show, Functor, Foldable, Traversable)
instance (Binary key, Binary keyDesc, Binary tree, Binary value)
  => Binary (Trie key keyDesc value tree)

