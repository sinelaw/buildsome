{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Buildsome.BuildMaps
  ( TargetRep(..), computeTargetRep
  , DirectoryBuildMap(..)
  , BuildMaps(..)
  , make
  , TargetKind(..)
  , find
  , findDirectory
  ) where


import Prelude.Compat hiding (FilePath)

import Control.Monad (mplus)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)

import Lib.FilePath (FilePath, takeDirectory)
import Lib.Makefile (Makefile(..), TargetType(..), Target, Pattern)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as M
import qualified Lib.Makefile as Makefile
import qualified Lib.StringPattern as StringPattern

newtype TargetRep = TargetRep { targetRepPath :: FilePath } -- We use the minimum output path as the target key/representative
  deriving (Eq, Ord, Show)
computeTargetRep :: Target -> TargetRep
computeTargetRep = TargetRep . minimum . targetOutputs

data DirectoryBuildMap = DirectoryBuildMap
  { dbmTargets :: [(TargetRep, Target)]
  , dbmPatterns :: [Pattern]
  } deriving (Show)
instance Monoid DirectoryBuildMap where
  mempty = DirectoryBuildMap mempty mempty
  mappend (DirectoryBuildMap x0 x1) (DirectoryBuildMap y0 y1) =
    DirectoryBuildMap (mappend x0 y0) (mappend x1 y1)

data BuildMaps = BuildMaps
  { _bmBuildMap :: Map FilePath (TargetRep, Target) -- output paths -> min(representative) path and original spec
  , _bmChildrenMap :: Map FilePath DirectoryBuildMap
  }

data TargetKind = TargetPattern | TargetSimple
  deriving (Eq)

find :: BuildMaps -> FilePath -> Maybe (TargetRep, TargetKind, Target)
find (BuildMaps buildMap childrenMap) path =
  -- Allow specific/simple matches to override pattern matches
  (set TargetSimple <$> simpleMatch) `mplus`
  (set TargetPattern <$> patternMatch)
  where
    set kind (rep, tgt) = (rep, kind, tgt)
    simpleMatch = path `M.lookup` buildMap
    patterns = dbmPatterns $ M.findWithDefault mempty (takeDirectory path) childrenMap
    instantiate pattern = (,) pattern <$> Makefile.instantiatePatternByOutput path pattern
    patternMatch =
      case mapMaybe instantiate patterns of
      [] -> Nothing
      [(_, target)] -> Just (computeTargetRep target, target)
      targets ->
        error $ BS8.unpack $ mconcat
        [ "Multiple matching patterns for: ", BS8.pack (show path), "\n"
        , BS8.unlines $
          map
          ( BS8.unwords .
            map (StringPattern.toString . Makefile.filePatternFile) .
            targetOutputs . fst
          )
          targets
        ]

findDirectory :: BuildMaps -> FilePath -> DirectoryBuildMap
findDirectory (BuildMaps _ childrenMap) path =
  M.findWithDefault mempty path childrenMap

make :: Makefile -> BuildMaps
make makefile = BuildMaps buildMap childrenMap
  where
    outputs =
      [ (outputPath, target)
      | target <- makefileTargets makefile
      , outputPath <- targetOutputs target
      ]
    childrenMap =
      M.fromListWith mappend $

      [ (takeDirectory outputPath, mempty { dbmTargets = [pairWithTargetRep target] })
      | (outputPath, target) <- outputs
      ] ++

      [ (outPatDir, mempty { dbmPatterns = [targetPattern] })
      | targetPattern <- makefilePatterns makefile
      , outPatDir <- nub (map Makefile.filePatternDirectory (targetOutputs targetPattern))
      ]

    pairWithTargetRep target = (computeTargetRep target, target)

    overlappingOutputs path (_, a) (_, b) =
      error $ "Overlapping output paths for: " ++ show path ++ " at:\n" ++
      show (targetPos a) ++ "vs.\n" ++ show (targetPos b)
    buildMap =
      M.fromListWithKey overlappingOutputs
      [ (outputPath, pairWithTargetRep target)
      | (outputPath, target) <- outputs ]
