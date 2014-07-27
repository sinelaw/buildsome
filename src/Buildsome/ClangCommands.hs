{-# LANGUAGE OverloadedStrings #-}
module Buildsome.ClangCommands
  ( make
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Trans.State (State, evalState)
import Data.Aeson ((.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Lib.BuildMaps (TargetRep)
import Lib.FilePath (FilePath, (</>))
import Lib.Makefile (TargetType(..), Target)
import Prelude hiding (FilePath)
import qualified Control.Monad.Trans.State as State
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BS8L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lib.BuildMaps as BuildMaps
import qualified Lib.Slave as Slave

-- Visited:
type M = State (Set TargetRep)

avoidRevisit :: TargetRep -> M a -> M (Maybe a)
avoidRevisit rep act = do
  visited <- State.get
  if rep `Set.member` visited
    then return Nothing
    else do
      State.modify $ Set.insert rep
      Just <$> act

buildCommands :: FilePath -> Slave.Stats -> Target -> M [Aeson.Value]
buildCommands cwd stats target =
  fmap (fromMaybe []) $
  avoidRevisit (BuildMaps.computeTargetRep target) $ do
    deps <- depBuildCommands
    return $ myBuildCommands ++ deps
  where
    myBuildCommands =
      case targetInputs target of
        [file]
          | not (BS8.null (targetCmds target)) ->
            [ Aeson.object
              [ "directory" .= BS8.unpack cwd
              , "command" .= BS8.unpack (targetCmds target)
              , "file" .= BS8.unpack file
              ]
            ]
        _ -> []
    depBuildCommands =
      case Map.lookup (BuildMaps.computeTargetRep target) (Slave.statsOfTarget stats) of
      Nothing ->
        error "BUG: Stats does not contain targets that appear as root/dependencies"
      Just (_, _, deps) -> buildCommandsTargets cwd stats deps

buildCommandsTargets :: FilePath -> Slave.Stats -> [Target] -> M [Aeson.Value]
buildCommandsTargets cwd stats = fmap concat . mapM (buildCommands cwd stats)

make :: FilePath -> Slave.Stats -> [Target] -> FilePath -> IO ()
make cwd stats rootTargets filePath = do
  putStrLn $ "Writing clang commands to: " ++ show (cwd </> filePath)
  BS8L.writeFile (BS8.unpack filePath) $
    encodePretty $ reverse $
    evalState (buildCommandsTargets cwd stats rootTargets) Set.empty
