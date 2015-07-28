{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, RankNTypes #-}
module Buildsome.Slave
    ( Slave, newWithUnmask
    , target
    , str
    , wait, waitCatch
    , cancel
    ) where

import           Prelude.Compat hiding (show, FilePath)

import qualified Buildsome.Color as Color

import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import           Data.Monoid
import           Lib.ColorText (ColorText)
import           Lib.FilePath (FilePath)
import           Lib.Makefile (Target)
import qualified Lib.Printer as Printer
import           Lib.Show (show)
import           Lib.TimeInstances ()

data Slave a = Slave
    { slaveTarget :: Target
    , slavePrinterId :: Printer.Id
    , slaveOutputPaths :: [FilePath]
    , slaveExecution :: a
    }

target :: Slave a -> Target
target = slaveTarget

newWithUnmask :: Target -> Printer.Id -> [FilePath] -> ((forall b. IO b -> IO b) -> IO a) -> IO (Slave a)
newWithUnmask tgt printerId outputPaths action =
    Slave tgt printerId outputPaths <$> (E.uninterruptibleMask $ \unmaskUninterruptible -> unmaskUninterruptible (action id))

    -- <$> Async.asyncWithUnmask
    --     -- NOTE: Using unmaskUninterruptible is not allowed in the
    --     -- child thread! However, it is impossible to put
    --     -- uninterruptibleMask just on the parent side of the thread
    --     -- creation while still allowing child to inherit a mask state
    --     -- EXCEPT using this undefined behavior. And without
    --     -- uninterruptibleMask wrapping of this, double async
    --     -- exception stops the exception handler, leaking threads.
    --     (\unmask -> unmaskUninterruptible (action unmask))

str :: Slave a -> ColorText
str slave =
    show (slavePrinterId slave) <> ": " <> cTarget (show (slaveOutputPaths slave))
    where
        Color.Scheme{..} = Color.scheme

wait :: Slave a -> IO a
wait = return . slaveExecution

waitCatch :: Slave a -> IO (Either E.SomeException a)
waitCatch = return . Right . slaveExecution

cancel :: Slave a -> IO ()
cancel = const $ return ()
