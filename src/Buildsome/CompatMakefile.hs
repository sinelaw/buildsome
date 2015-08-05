{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Buildsome.CompatMakefile
  ( Phonies, make
  ) where

import Prelude.Compat hiding (FilePath)

import Buildsome.BuildMaps (TargetRep)
import Buildsome.Stats (Stats)

import Control.Monad (filterM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Set (Set)
import Lib.FilePath (FilePath, (</>))
import Lib.Makefile (TargetType(..), Target)
import Lib.Parsec (showPos)
import qualified Buildsome.BuildMaps as BuildMaps
import qualified Buildsome.Stats as Stats
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lib.Directory as Directory
import qualified Lib.Revisit as Revisit
import qualified Lib.FilePath as FilePath
import qualified System.Posix.ByteString as Posix

isDir :: FilePath -> IO Bool
isDir path = maybe False Posix.isDirectory <$> Directory.getMFileStatus path

type M = Revisit.M TargetRep IO

data MakefileTarget = MakefileTarget
  { makefileTargetPath :: FilePath
  , makefileTargetDirs :: [FilePath]
  , isDirectory :: Bool
  }

makefileTarget :: Target -> IO MakefileTarget
makefileTarget target = do
  repIsDir <- isDir repPath
  targetDirs <- filterM isDir (targetOutputs target)
  return MakefileTarget
    { makefileTargetPath = if repIsDir then repPath </> ".dir" else repPath
    , makefileTargetDirs = targetDirs
    , isDirectory = repIsDir
    }
  where
    repPath = BuildMaps.targetRepPath $ BuildMaps.computeTargetRep target

makefileTargetDirsBS :: MakefileTarget -> [BS8.ByteString]
makefileTargetDirsBS = map FilePath.toBS . makefileTargetDirs

makefileTargetPathBS :: MakefileTarget -> BS8.ByteString
makefileTargetPathBS = FilePath.toBS . makefileTargetPath

targetCmdLines :: MakefileTarget -> TargetType output input -> [BS8.ByteString]
targetCmdLines tgt target =
  ["rm -rf " <> dir | dir <- makefileTargetDirsBS tgt] ++
--  (BS8.lines . targetCmds) target ++
  (BS8.lines . targetCmds) target ++
  ["touch " <> makefileTargetPathBS tgt | isDirectory tgt]

type Phonies = Set FilePath

onOneTarget :: Set BS8.ByteString -> FilePath -> Stats -> Target -> M [BS8.ByteString]
onOneTarget phoniesSet cwd stats target =
  fmap (fromMaybe []) $
  Revisit.avoid targetRep $ do
    depsLines <- depBuildCommands
    tgt <- lift $ makefileTarget target
    depTgts <- lift $ mapM makefileTarget directDeps
    let
      targetDecl = mconcat
        [ "T := ", makefileTargetPathBS tgt, "\n$(T):"
        , spaceUnwords $ map makefileTargetPathBS depTgts
        ]
      myLines =
        [ "#" <> BS8.pack (showPos (targetPos target)) ] ++
        [ ".PHONY: " <> makefileTargetPathBS tgt
        | makefileTargetPathBS tgt `Set.member` phoniesSet
        ] ++
        [ targetDecl ] ++
        map ("\t" <>) (targetCmdLines tgt target) ++
        [ "" ]
    return $ myLines ++ depsLines
  where
    spaceUnwords = BS8.concat . map (" " <>)
    targetRep = BuildMaps.computeTargetRep target
    directDeps = Stats.tsDirectDeps targetStats
    targetStats =
      fromMaybe (error "BUG: Stats does not contain targets that appear as root/dependencies") $
      Map.lookup targetRep (Stats.ofTarget stats)
    depBuildCommands = onMultipleTargets phoniesSet cwd stats directDeps

onMultipleTargets :: Set BS8.ByteString -> FilePath -> Stats -> [Target] -> M [BS8.ByteString]
onMultipleTargets phoniesSet cwd stats = fmap concat . mapM (onOneTarget phoniesSet cwd stats)

make :: Phonies -> FilePath -> Stats -> [Target] -> FilePath -> IO ()
make phoniesSet cwd stats rootTargets filePath = do
  putStrLn $ "Writing compat makefile to: " ++ show (cwd </> filePath)
  makefileLines <- Revisit.run (onMultipleTargets (Set.map FilePath.toBS phoniesSet) cwd stats rootTargets)
  BS8.writeFile (FilePath.toString filePath) $
    BS8.unlines $
    [ "# Auto-generated compatibility mode Makefile"
    , "# THIS MAKEFILE IS INVALID AS SOON AS ANY CHANGE OCCURS ANYWHERE"
    , "# ON THE FILE SYSTEM (even outside your project). USE CAREFULLY."
    , "# make -f compat-makefile"
    ] ++ makefileLines
