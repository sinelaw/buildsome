{-# LANGUAGE CPP, FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Lib.TimeInstances () where

import Control.Applicative ((<$>))
import Data.Binary(Binary(..), Get(..), Put)
import Data.Time.Clock (NominalDiffTime, DiffTime)
import Data.Fixed (Pico, Fixed(..), E12)
import Data.Int(Int64)

put' :: RealFrac a => a -> Put
put' x = 
    do let (a, b) = properFraction x
       put $! (a :: Int64)
       put $! (truncate (b * 10^12) :: Int64)

get' :: RealFrac a => Get a
get' = 
    do a <- get :: Get Int64
       bScaled <- get :: Get Int64
       return $! fromIntegral a + (fromIntegral bScaled / 10^12)

instance Binary DiffTime where
  put = put' :: DiffTime -> Put
  get = get' :: Get DiffTime
  {-# INLINE put #-}
  {-# INLINE get #-}

instance Binary NominalDiffTime where
  put = put' :: NominalDiffTime -> Put 
  get = get' :: Get NominalDiffTime
  {-# INLINE put #-}
  {-# INLINE get #-}
  
