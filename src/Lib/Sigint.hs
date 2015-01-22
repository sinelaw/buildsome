module Lib.Sigint (installSigintHandler) where

import Control.Concurrent (myThreadId)
import System.Posix.ByteString (Handler(..), keyboardSignal, installHandler)
import qualified Control.Exception as E

installSigintHandler :: IO ()
installSigintHandler = do
  tid <- myThreadId
  let handler = putStrLn ("keyboardSignal in thread: " ++ show tid) >> E.throwTo tid E.UserInterrupt
  _ <- installHandler keyboardSignal (Catch handler) Nothing
  return ()
