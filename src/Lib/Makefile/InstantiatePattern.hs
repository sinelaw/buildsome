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

plugFilePattern :: StringPattern.Match -> FilePattern -> Maybe FilePath
plugFilePattern match (FilePattern dir file) = (dir </>) <$> StringPattern.plug match file

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
instantiatePatternByOutput outputPath target = {-# SCC "instantiatePatternByOutput" #-}
  msum $ map tryMatchOutput (targetOutputs target)
  where
    (outputDir, outputFile) = splitFileName outputPath
    tryMatchOutput (FilePattern patDir patFile) = {-# SCC "instantiatePatternByOutput.tryMatchOutput" #-} do
      guard (patDir == outputDir)
      outputMatch <- StringPattern.match patFile outputFile
      instantiatePatternByMatch outputMatch target
