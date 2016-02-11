{-# LANGUAGE BangPatterns #-}
module Lib.TimeIt
    ( timeIt
    , printTimeIt
    , pureTimeIt
    ) where

import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import System.IO.Unsafe

timeIt :: MonadIO io => io a -> io (NominalDiffTime, a)
timeIt act =
    do
        before <- liftIO $ getCurrentTime
        !res <- act
        after <- liftIO $ getCurrentTime
        return (after `diffUTCTime` before, res)

printTimeIt :: MonadIO io => String -> io a -> io a
printTimeIt msg act =
    do
        (t, res) <- timeIt act
        liftIO $ putStrLn $ msg ++ " took " ++ show t
        return res

pureTimeIt :: String -> a -> a
pureTimeIt msg x = unsafePerformIO $ printTimeIt msg (evaluate x)
