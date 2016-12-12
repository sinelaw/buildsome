{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Buildsome.BuildMaps
  ( TargetRep(..), computeTargetRep
  , TargetDesc(..), descOfTarget
  , DirectoryBuildMap(..)
  , BuildMaps(..)
  , make
  , TargetKind(..)
  , find
  , findDirectory
  ) where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS8
import           Data.List (nub)
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import           Lib.FilePath (FilePath, takeDirectory)
import           Lib.Makefile (Makefile(..), TargetType(..), Pattern
                              , TargetRep(..), TargetDesc(..), descOfTarget, computeTargetRep
                              , TargetKind(..))
import qualified Lib.Makefile as Makefile
import qualified Lib.StringPattern as StringPattern
import           Lib.Parsec (showPos)

import           Text.Parsec (SourcePos)
import           Prelude.Compat hiding (FilePath)

data DirectoryBuildMap = DirectoryBuildMap
  { dbmTargets :: [TargetDesc]
  , dbmPatterns :: [Pattern]
  } deriving (Show)
instance Monoid DirectoryBuildMap where
  mempty = DirectoryBuildMap mempty mempty
  mappend (DirectoryBuildMap x0 x1) (DirectoryBuildMap y0 y1) =
    DirectoryBuildMap (mappend x0 y0) (mappend x1 y1)

data BuildMaps = BuildMaps
  { _bmBuildMap :: Map FilePath TargetDesc -- output paths -> min(representative) path and original spec
  , _bmChildrenMap :: Map FilePath DirectoryBuildMap
  }

posText :: (Monoid s, IsString s) => SourcePos -> s
posText pos = mconcat [fromString (showPos pos), ": "]

find :: BuildMaps -> FilePath -> Maybe (TargetKind, TargetDesc)
find (BuildMaps buildMap childrenMap) path =
  -- Allow specific/simple matches to override pattern matches
  ((,) TargetSimple <$> simpleMatch) `mplus`
  ((,) TargetPattern <$> patternMatch)
  where
    simpleMatch = (path `M.lookup` buildMap)
    patterns = dbmPatterns $ M.findWithDefault mempty (takeDirectory path) childrenMap
    instantiate pattern = (,) pattern <$> Makefile.instantiatePatternByOutput path pattern
    patternMatch =
      case mapMaybe instantiate patterns of
      [] -> Nothing
      [(_, target)] -> Just $ descOfTarget target
      targets ->
        error $ BS8.unpack $ mconcat
        [ "Multiple matching patterns for: ", BS8.pack (show path), "\n"
        , BS8.unlines $
          map (showPattern . fst) targets
        ]
    showPattern pattern =
      posText (targetPos pattern) <> showPatternOutputs pattern
    showPatternOutputs pattern =
      BS8.unwords $
      map (StringPattern.toString . Makefile.filePatternFile) $
      targetOutputs pattern

findDirectory :: BuildMaps -> FilePath -> DirectoryBuildMap
findDirectory (BuildMaps _ childrenMap) path =
  M.findWithDefault mempty path childrenMap

make :: Makefile -> BuildMaps
make makefile = BuildMaps buildMap childrenMap
  where
    outputs =
      [ (outputPath, descOfTarget target)
      | target <- makefileTargets makefile
      , outputPath <- targetOutputs target
      ]
    childrenMap =
      M.fromListWith mappend $
      [ (takeDirectory outputPath, mempty { dbmTargets = [targetDesc] })
      | (outputPath, targetDesc) <- outputs
      ] ++

      [ (outPatDir, mempty { dbmPatterns = [targetPattern] })
      | targetPattern <- makefilePatterns makefile
      , outPatDir <- nub (map Makefile.filePatternDirectory (targetOutputs targetPattern))
      ]

    overlappingOutputs path (TargetDesc _ a) (TargetDesc _ b) =
      error $ "Overlapping output paths for: " ++ show path ++ " at:\n" ++
      show (targetPos a) ++ "vs.\n" ++ show (targetPos b)
    buildMap =
      M.fromListWithKey overlappingOutputs outputs
