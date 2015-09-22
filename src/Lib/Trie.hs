{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Trie
       ( Trie(..)
       , prettyTrieSummary
       )
       where

import Lib.NonEmptyMap (NonEmptyMap)
import qualified Lib.NonEmptyMap as NonEmptyMap
import Lib.NonEmptyList (NonEmptyList(..))

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


prettyTrieSummary ::
  (Show key) =>
  (tree -> Trie key keyDesc value tree) ->
  tree ->
  String
prettyTrieSummary unfix = go ""
  where
    go indent node =
      case unfix node of
      Leaf{} -> "Leaf"
      Branch m ->
        mconcat
        [ "\n", indent, ('>' :) . concatMap (uncurry showInput) $ NonEmptyMap.toList m ]
        where
          showInput input branches =
            case NonEmptyMap.toNonEmptyList branches of
            NonEmptyList x [] -> mconcat [show input, go t $ snd x]
            NonEmptyList x xs ->
                -- TODO: List comprehension with unlines
                concatMap
                ((mconcat ["\n", t, show input, ":"] ++) . go t . snd)
                (x:xs)
          t = ' ' : indent
