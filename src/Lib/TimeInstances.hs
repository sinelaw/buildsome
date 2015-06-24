{-# LANGUAGE CPP, FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Lib.TimeInstances () where

import Control.Applicative ((<$>))
import Data.Binary(Binary(..))
import Data.Binary.Get (getWord64host)
import Data.Binary.Put (putWord64host)
import Data.Time.Clock (NominalDiffTime, DiffTime)
import Data.Fixed (Pico, Fixed(..), E12)
import Data.Word (Word64)

toPicos :: Pico -> Word64
fromPicos :: Word64 -> Pico
toPicos = truncate . (*1e12)
fromPicos = (/1e12) . realToFrac

{-# INLINE toPicos #-}
{-# INLINE fromPicos #-}

instance Binary (Fixed E12) where
  get = fromPicos <$> getWord64host
  put = putWord64host . toPicos
  {-# INLINE get #-}
  {-# INLINE put #-}

{-# INLINE toPico #-}
toPico :: Real a => a -> Pico
toPico = realToFrac

{-# INLINE fromPico #-}
fromPico :: Fractional a => Pico -> a
fromPico = realToFrac

instance Binary NominalDiffTime where
  put = put . toPico
  get = fromPico <$> get
  {-# INLINE get #-}
  {-# INLINE put #-}

instance Binary DiffTime where
  put = put . toPico
  get = fromPico <$> get
  {-# INLINE get #-}
  {-# INLINE put #-}
