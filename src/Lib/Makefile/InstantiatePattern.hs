{-# LANGUAGE NoImplicitPrelude #-}
module Lib.Makefile.InstantiatePattern
  ( instantiatePatternByOutput
  , instantiatePatternByMatch
  ) where


import Prelude.Compat hiding (FilePath)

import Control.Monad (guard, msum)
import Lib.FilePath ((</>), FilePath, splitFileName)
import Lib.Makefile.Parser (interpolateCmds)
import Lib.Makefile.Types
import qualified Lib.StringPattern as StringPattern
import qualified Lib.FilePath as FilePath

plugFilePattern :: StringPattern.Match -> FilePattern -> Maybe FilePath
plugFilePattern match (FilePattern dir file) = ((dir </>) . FilePath.fromBS) <$> StringPattern.plug match file

instantiatePatternByMatch :: StringPattern.Match -> Pattern -> Maybe Target
instantiatePatternByMatch match (Target outputs inputs ooInputs cmds pos) =
  interpolateCmds mStem <$>
  ( Target
    <$> mPluggedOutputs
    <*> mPluggedInputs
    <*> mPluggedOOInputs
    <*> pure cmds
    <*> pure pos
  )
  where
    mStem = Just $ StringPattern.matchPlaceHolder1 match
    plugInputMatch (InputPattern pat) = plugFilePattern match pat
    plugInputMatch (InputPath str) = Just str
    mPluggedOutputs  = mapM (plugFilePattern match) outputs
    mPluggedInputs   = mapM plugInputMatch inputs
    mPluggedOOInputs = mapM plugInputMatch ooInputs

instantiatePatternByOutput :: FilePath -> Pattern -> Maybe Target
instantiatePatternByOutput outputPath target =
  msum $ map tryMatchOutput (targetOutputs target)
  where
    (outputDir, outputFile) = splitFileName outputPath
    tryMatchOutput (FilePattern patDir patFile) = do
      guard (patDir == outputDir)
      outputMatch <- StringPattern.match patFile (FilePath.toBS outputFile)
      instantiatePatternByMatch outputMatch target
