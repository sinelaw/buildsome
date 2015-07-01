{-# LANGUAGE OverloadedStrings #-}
module Lib.Parallelism
  ( ParId
  , Priority(..)
  , Token, rootToken
  , ChildManager, newChildManager
  , WaitContext, newWaitContext, startFork, withReleased
  ) where

-- -- import           Control.Concurrent.MVar
-- -- import qualified Control.Exception as E
-- -- import           Control.Monad
-- -- import           Data.IORef
-- -- import           Lib.Exception (bracket, bracket_, finally)
-- -- import           Lib.IORef (atomicModifyIORef_)
import           Lib.PoolAlloc (Priority(..))
-- -- import qualified Lib.PoolAlloc as PoolAlloc
-- -- import           Lib.Printer (Printer)
-- -- import qualified Lib.Printer as Printer

type ParId = Int
-- type Pool = PoolAlloc ParId

-- NOTE: withReleased may be called multiple times on the same Cell,
-- concurrently. This is allowed, but the parallelism will only be
-- released and regained once. The regain will occur after all
-- withReleased sections completed. This means that not every
-- "withReleased" completion actually incurs a re-allocation -- so
-- withReleased can complete without parallelism being allocated. This
-- happens anywhere whenever there is hidden concurrency in a build
-- step, so it's not a big deal.

data Token
data ChildManager -- represents a full "job" in buildsome (target execution)
data WaitContext  -- represents a single "wait" for children

rootToken :: ParId -> IO Token
rootToken _n = undefined -- PoolAlloc.new [1..n]

newChildManager :: Token -> IO ChildManager
newChildManager = undefined

newWaitContext :: ChildManager -> IO WaitContext
newWaitContext = undefined

-- This must be used exactly once on the WaitContext, after which it is unusable
withReleased :: WaitContext -> Priority -> IO a -> IO a
withReleased = undefined

startFork :: WaitContext -> Priority -> IO ((Token -> IO r) -> IO r)
startFork = undefined
