{-# LANGUAGE OverloadedStrings #-}
module Lib.Parallelism
  ( ParId
  , Priority(..)
  , TokenCell, rootTokenCell
  , WaitContext, newWaitContext, startFork, withReleased
  ) where

-- -- import           Control.Concurrent.MVar
-- -- import qualified Control.Exception as E
-- -- import           Control.Monad
import           Data.IORef
-- -- import           Lib.Exception (bracket, bracket_, finally)
-- -- import           Lib.IORef (atomicModifyIORef_)
import           Lib.PoolAlloc (PoolAlloc, Priority(..))
import qualified Lib.PoolAlloc as PoolAlloc
-- -- import           Lib.Printer (Printer)
-- -- import qualified Lib.Printer as Printer

type ParId = Int
-- type Pool = PoolAlloc ParId

-- data TokenCellState
--     = TokenCellAlloced ParId
--     | TokenCellReleased

data TokenCell = TokenCell
    { tokenPool :: PoolAlloc ParId
--    , tokenParId :: IORef TokenCellState
    }

-- | represents a single "wait" for children
data WaitContext = WaitContext
    { _wcTokenCell :: TokenCell
    }

rootTokenCell :: ParId -> IO TokenCell
rootTokenCell n =
    do
        pool <- PoolAlloc.new [1..n]
        _parId <- newIORef =<< PoolAlloc.alloc (Priority 0) pool
        return TokenCell
            { tokenPool = pool
--            , tokenParId = parId
            }

newWaitContext :: TokenCell -> IO WaitContext
newWaitContext = return . WaitContext

-- NOTE: withReleased may be called multiple times on the same Cell,
-- concurrently. This is allowed, but the parallelism will only be
-- released and regained once. The regain will occur after all
-- withReleased sections completed. This means that not every
-- "withReleased" completion actually incurs a re-allocation -- so
-- withReleased can complete without parallelism being allocated. This
-- happens anywhere whenever there is hidden concurrency in a build
-- step, so it's not a big deal.

-- This must be used exactly once on the WaitContext, after which it is unusable
withReleased :: WaitContext -> Priority -> IO a -> IO a
withReleased _waitContext _priority _body = undefined
    -- bracket

startFork :: WaitContext -> Priority -> IO ((TokenCell -> IO r) -> IO r)
startFork = undefined
