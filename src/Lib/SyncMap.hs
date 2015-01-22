{-# LANGUAGE TupleSections #-}
module Lib.SyncMap
    ( SyncMap(..)
    , Inserted(..)
    , Result
    , tryInsert
    , insert
    , new
    ) where

import           Control.Concurrent.MVar
import           Control.Exception       (SomeException, mask_, try, throwIO)
import           Data.Functor            ((<$>))
import           Data.IORef              (IORef, atomicModifyIORef, newIORef)
import qualified Data.Map.Strict         as M
import qualified Lib.Exception           as E

type Result = Either SomeException

newtype SyncMap k a = SyncMap { getSyncMap :: IORef (M.Map k (MVar (Result a))) }

data Inserted = Inserted | Old

-- | Safely insert (or update) a value into a SyncMap.
--
-- Uses masking internally to ensure one of two outcomes:
--
-- 1. The map is updated: if the key is missing from the map, the given action is run in an uninterruptibleMask, and the resulting value is inserted
-- 2. The map is not updated: if the key exists, returns the old value.
--
-- If an exception occurred during the synchronous update, it will be returned in the `Result`.
--
tryInsert :: Ord k => SyncMap k a -> k -> IO a -> IO (Inserted, Result a)
tryInsert (SyncMap refMap) key action =
  do mvar <- newEmptyMVar
     let fillMVar x =
           do putMVar mvar x
              return (Inserted, x)
     mask_ $
       do scheduledAction <-
            atomicModifyIORef refMap $
              \oldMap ->
                case M.lookup key oldMap of
                  Just oldMVar -> (oldMap, (Old,) <$> readMVar oldMVar)
                  Nothing ->
                    ( M.insert key mvar oldMap
                    , E.loggedUninterruptibleMask_ $ try action >>= fillMVar
                    )
          scheduledAction


-- | Version of tryInsert that throws exceptions instead of returning them in a `Result`, and also discards the `Inserted` value
insert :: Ord k => SyncMap k a -> k -> IO a -> IO a
insert syncMap key action =
  do (_, res) <- tryInsert syncMap key action
     either throwIO return res


new :: IO (SyncMap k v)
new = SyncMap <$> newIORef M.empty

