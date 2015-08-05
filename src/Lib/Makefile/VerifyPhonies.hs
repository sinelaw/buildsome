{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Lib.Makefile.VerifyPhonies
  ( verifyPhonies
  ) where

import Prelude.Compat hiding (FilePath)
import Data.Typeable (Typeable)
import Lib.Parsec (showPos)
import qualified Control.Exception as E
import qualified Data.Set as S
import qualified Text.Parsec.Pos as Pos
import Lib.Makefile.Types (Makefile(..), TargetType(..))
import qualified Lib.FilePath as FilePath
import Lib.FilePath (FilePath)

data MissingPhony = MissingPhony Pos.SourcePos FilePath deriving (Typeable)
instance E.Exception MissingPhony
instance Show MissingPhony where
  show (MissingPhony pos danglingInput) =
    concat [showPos pos, ": ", ".PHONY refers to inexistent target " ++ FilePath.toString danglingInput]

verifyPhonies :: Makefile -> IO ()
verifyPhonies makefile =
  case filter ((`S.notMember` outputPathsSet) . snd) (makefilePhonies makefile) of
  [] -> return ()
  ((pos, danglingInput):_) -> E.throwIO (MissingPhony pos danglingInput)
  where
    outputPathsSet = S.fromList $ concatMap targetOutputs $ makefileTargets makefile
