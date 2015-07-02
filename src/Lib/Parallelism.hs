{-# LANGUAGE OverloadedStrings #-}
module Lib.Parallelism
  ( ParId
  , Pool, newPool
  , Priority(..)
  , Fork(..), fork, wrapForkedChild
  , TokenCell, rootTokenCell
  , withReleased
  ) where

-- -- import           Control.Concurrent.MVar
-- -- import qualified Control.Exception as E
-- -- import           Control.Monad
import           Data.IORef
import           Lib.Exception (bracket, bracket_, finally)
-- -- import           Lib.IORef (atomicModifyIORef_)
import           Lib.PoolAlloc (PoolAlloc, Priority(..))
import qualified Lib.PoolAlloc as PoolAlloc
-- -- import           Lib.Printer (Printer)
-- -- import qualified Lib.Printer as Printer

type ParId = Int
type Pool = PoolAlloc ParId

data TokenCellState
    = TokenCellAlloced ParId
    -- | TokenCellReleasedToChildren Int
    -- | TokenCellFinished -- released back to parent

newtype TokenCell = TokenCell
    { tokenCellState :: IORef TokenCellState
    }

data WaitContext = WaitContext

data Fork = Fork
    { forkPool :: Pool
    , forkParents :: IORef [WaitContext]
    , forkFinishAlloc :: IO ParId
    }

newPool :: ParId -> IO Pool
newPool n = PoolAlloc.new [1..n]

rootTokenCell :: Pool -> IO TokenCell
rootTokenCell pool =
    do
        state <- newIORef . TokenCellAlloced =<< PoolAlloc.alloc (Priority 0) pool
        return $ TokenCell state

connectForksToParent :: WaitContext -> [Fork] -> IO ()
connectForksToParent _waitContext _forks = undefined

releaseToken :: TokenCell -> IO ()
releaseToken _token = undefined

regainToken :: WaitContext -> IO ()
regainToken _waitContext = undefined

-- NOTE: withReleased may be called multiple times on the same Cell,
-- concurrently. This is allowed, but the parallelism will only be
-- released and regained once. The regain will occur after all
-- withReleased sections completed. This means that not every
-- "withReleased" completion actually incurs a re-allocation -- so
-- withReleased can complete without parallelism being allocated. This
-- happens anywhere whenever there is hidden concurrency in a build
-- step, so it's not a big deal.

withReleased :: TokenCell -> Priority -> [Fork] -> IO a -> IO a
withReleased token priority forks =
    do
        waitContext <- return WaitContext
        let beforeRelease =
                do
                    connectForksToParent waitContext forks
                    releaseToken token
            afterRelease = regainToken waitContext -- we already got it from children
        bracket_ beforeRelease afterRelease

wrapForkedChild :: Fork -> (TokenCell -> IO r) -> IO r
wrapForkedChild child =
    bracket beforeChild afterChild
    where
        pool = forkPool child
        beforeChild = fmap TokenCell . newIORef . TokenCellAlloced =<< forkFinishAlloc child
        afterChild token =
            do
                state <- readIORef $ tokenCellState token
                case state of
                    TokenCellAlloced parId ->
                        do
                            -- allocForParents??
                            PoolAlloc.release pool parId
                    -- _ -> error "Child lost its token!"

-- | Must call wrapForkedChild at the child context!
fork :: Pool -> Priority -> IO Fork
fork pool priority =
    do
        finishAlloc <- PoolAlloc.startAlloc priority pool
        parents <- newIORef []
        return Fork
            { forkPool = pool
            , forkParents = parents
            , forkFinishAlloc = finishAlloc
            }
