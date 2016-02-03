{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib.Makefile.Types
  ( TargetType(..), targetAllInputs
  , FilePattern(..), onFilePatternPaths
  , InputPat(..), onInputPatPaths
  , Target, onTargetPaths
  , TargetRep(..), computeTargetRep
  , TargetDesc(..), descOfTarget
  , TargetKind(..)
  , Pattern, onPatternPaths
  , VarName, VarValue, Vars
  , Makefile(..), onMakefilePaths
  ) where


import Prelude.Compat hiding (FilePath)

import Control.DeepSeq (NFData(..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Map (Map)

import GHC.Generics (Generic)
import Lib.FilePath (FilePath)
import Lib.Parsec () -- instance Binary SourcePos
import Lib.StringPattern (StringPattern)
import qualified Text.Parsec.Pos as ParsecPos

data TargetType output input = Target
  { targetOutputs :: [output]
  , targetInputs :: [input]
  , targetOrderOnlyInputs :: [input]
  , targetCmds :: ByteString
  , targetPos :: ParsecPos.SourcePos
  } deriving (Show, Generic)
instance (Binary output, Binary input) => Binary (TargetType output input)
instance (NFData output, NFData input) => NFData (TargetType output input) where
  rnf = genericRnf

type Target = TargetType FilePath FilePath

-- | Unique identifier of the target.
newtype TargetRep = TargetRep { targetRepPath :: FilePath } -- We use the minimum output path as the
                                                            -- target key/representative. It's ok to
                                                            -- do this because each target outputs
                                                            -- can't overlap
  deriving (Eq, Ord, Show, Generic)
instance Binary TargetRep
instance NFData TargetRep where rnf = genericRnf

data TargetDesc = TargetDesc
  { tdRep :: TargetRep
  , tdTarget :: Target
  } deriving (Show, Generic)
instance Binary TargetDesc
instance NFData TargetDesc where rnf = genericRnf

computeTargetRep :: Target -> TargetRep
computeTargetRep = {-# SCC "computeTargetRep" #-} (TargetRep . minimum . targetOutputs)

descOfTarget :: Target -> TargetDesc
descOfTarget target = TargetDesc (computeTargetRep target) target

data TargetKind = TargetPattern | TargetSimple
  deriving (Eq, Show, Generic)
instance Binary TargetKind
instance NFData TargetKind where rnf = genericRnf

data FilePattern = FilePattern
  { filePatternDirectory :: FilePath
  , filePatternFile :: StringPattern
  } deriving (Show, Generic)
instance Binary FilePattern
instance NFData FilePattern where rnf = genericRnf

data InputPat = InputPath FilePath | InputPattern FilePattern
  deriving (Show, Generic)
instance Binary InputPat
instance NFData InputPat where rnf = genericRnf

type Pattern = TargetType FilePattern InputPat

type VarName = ByteString
type VarValue = ByteString
type Vars = Map VarName VarValue

data Makefile = Makefile
  { makefileTargets :: [Target]
  , makefilePatterns :: [Pattern]
  , makefilePhonies :: [(ParsecPos.SourcePos, FilePath)]
  , makefileWeakVars :: Vars
  } deriving (Show, Generic)
instance Binary Makefile
instance NFData Makefile where rnf = genericRnf

targetAllInputs :: Target -> [FilePath]
targetAllInputs target =
  targetInputs target ++ targetOrderOnlyInputs target

-- Filepath lens boilerplate:

onTargetPaths :: Applicative f => (FilePath -> f FilePath) -> Target -> f Target
onTargetPaths f (Target outputs inputs orderOnlyInputs cmds pos) =
  Target
  <$> traverse f outputs
  <*> traverse f inputs
  <*> traverse f orderOnlyInputs
  <*> pure cmds
  <*> pure pos

onFilePatternPaths :: Functor f => (FilePath -> f FilePath) -> FilePattern -> f FilePattern
onFilePatternPaths f (FilePattern dir file) = (`FilePattern` file) <$> f dir

onInputPatPaths :: Functor f => (FilePath -> f FilePath) -> InputPat -> f InputPat
onInputPatPaths f (InputPath x) = InputPath <$> f x
onInputPatPaths f (InputPattern x) = InputPattern <$> onFilePatternPaths f x

onPatternPaths :: Applicative f => (FilePath -> f FilePath) -> Pattern -> f Pattern
onPatternPaths f (Target outputs inputs orderOnlyInputs cmds pos) =
  Target
  <$> traverse (onFilePatternPaths f) outputs
  <*> traverse (onInputPatPaths f) inputs
  <*> traverse (onInputPatPaths f) orderOnlyInputs
  <*> pure cmds
  <*> pure pos

_2 :: Functor f => (b -> f c) -> (a, b) -> f (a, c)
_2 f (x, y) = (,) x <$> f y

onMakefilePaths :: Applicative f => (FilePath -> f FilePath) -> Makefile -> f Makefile
onMakefilePaths f (Makefile targets patterns phonies weakVars) =
  Makefile
  <$> traverse (onTargetPaths f) targets
  <*> traverse (onPatternPaths f) patterns
  <*> (traverse . _2) f phonies
  <*> pure weakVars
