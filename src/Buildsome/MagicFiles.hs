{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Buildsome.MagicFiles
  ( inputIgnored, outputIgnored
  , allowedUnspecifiedOutput
  ) where

import Prelude.Compat hiding (FilePath)

import Lib.FilePath (FilePath)
import qualified Lib.FilePath as FilePath
import qualified Data.ByteString.Char8 as BS8

specialFile :: FilePath -> Bool
specialFile path = any (`BS8.isPrefixOf` path') ["/dev", "/proc", "/sys", "/var/folders"]
  where path' = FilePath.toBS path

inputIgnored :: FilePath -> Bool
inputIgnored = specialFile

outputIgnored :: FilePath -> Bool
outputIgnored = specialFile

allowedUnspecifiedOutput :: FilePath -> Bool
allowedUnspecifiedOutput fp = ".pyc" `BS8.isSuffixOf` (FilePath.toBS fp)
