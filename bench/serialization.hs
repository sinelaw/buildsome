{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Prelude.Compat          hiding (FilePath)

import Lib.Hash
import Lib.StdOutputs(StdOutputs(..))
import           Buildsome.BuildId (BuildId(..))
import Control.DeepSeq (NFData(..))
import           Data.ByteString.Lazy (ByteString)
import qualified Buildsome.Db            as Db
import           Control.Monad           (forM)
import           Control.Monad.IO.Class  (MonadIO (..))
import qualified Lib.Directory           as Dir
import           Lib.FileDesc            (FileContentDesc (..), FileDesc (..),
                                          fileContentDescOfStat,
                                          fileModeDescOfStat,
                                          fileStatDescOfStat)
import           Lib.FilePath            (FilePath)
import qualified System.Posix.ByteString as Posix

import           Data.Binary
-- import           System.TimeIt

import Criterion.Main

getFileDescInput :: MonadIO m => Db.Reason -> FilePath -> m Db.InputDesc
getFileDescInput reason filePath = {-# SCC "getFileDescInput" #-} do
  mStat <- liftIO $ Dir.getMFileStatus filePath
  case mStat of
      Nothing -> return $ Db.InputDescOfNonExisting reason
      Just stat -> do
          contentDesc <- liftIO $ fileContentDescOfStat filePath stat
          let time = Posix.modificationTimeHiRes stat
          return
              $ Db.InputDescOfExisting time $ Db.ExistingInputDescOf
              { Db.idModeAccess = Just (reason, fileModeDescOfStat stat)
              , Db.idStatAccess = Just (reason, fileStatDescOfStat stat)
              , Db.idContentAccess = Just (reason, contentDesc)
              }


-- measure :: Show a => Int -> String -> IO a -> IO ()
-- measure num msg act = do
--     (time, _) <- timeItT $ forM [1..num] $ \n -> do
--         res <- act
--         return $! length (show res)
--     putStrLn $ msg ++ ": " ++ (show $ 1000000 * time / (fromIntegral num)) ++ " micros"

main = do
    -- measure 1000 "getMFileStatus /tmp" (fmap Posix.fileSize <$> Dir.getMFileStatus "/tmp")
    -- measure 100 "getFileDescInput /tmp" (getFileDescInput (Db.BecauseHintFrom []) "/tmp")
    -- measure 100 "getFileDescInput /bin/bash" (getFileDescInput (Db.BecauseHintFrom []) "/bin/bash")
    -- measure 10000 "encode" (return $! encode testValue)
    -- measure 10000 "decode" (return $! decode encoded :: IO Db.InputDesc)
    let dummyKey = Db.StringKeyShort "dummy"

    testValue <- fmap (const dummyKey) <$> getFileDescInput (Db.BecauseHintFrom []) "/tmp"

    let !encoded = encode testValue
        !executionLog =
            Db.ExecutionLogOf
            { Db.elBuildId = BuildId "dummy"
            , Db.elCommand = dummyKey
            , Db.elInputBranchPath = Db.ELBranchPath $ map (uncurry Db.ELBranchInput) . take 1000 $ repeat (dummyKey, testValue)
            , Db.elOutputsDescs = [(dummyKey, FileDescNonExisting ())]
            , Db.elStdoutputs = StdOutputs dummyKey dummyKey
            , Db.elSelfTime = 0
            } :: Db.ExecutionLogOf Db.StringKey
        encodedExecutionLog = encode executionLog

    Db.with "Buildsome.mk.db" $ \db -> do
        let elKey = Db.ExecutionLogNodeKey $ md5 "1234"
            elIRef = Db.executionLogNode elKey db
        defaultMain
            [ bgroup "InputDesc"
              [ bench "encode" $ nf encode testValue
              , bench "decode" $ nf (decode :: ByteString -> Db.InputDescOf Db.StringKey) encoded ]

            , bgroup "ExecutionLogForDb"
              [ bench "encode" $ nf encode executionLog
              , bench "decode" $ nf (decode :: ByteString -> Db.ExecutionLogOf Db.StringKey) encodedExecutionLog ]

            , bgroup "Db"
              [ bench "write" $ nfIO (Db.writeIRef elIRef (Db.ExecutionLogNodeLeaf executionLog))
              , bench "read" $ nfIO (Db.readIRef elIRef)
              ]
            ]

