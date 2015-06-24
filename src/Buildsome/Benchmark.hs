module Buildsome.Benchmark where

import Buildsome.Db
import Lib.Makefile (Target(..), TargetType(..))
import Data.Binary(Binary(..))
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Char8 as BS8
import qualified Text.Parsec.Pos as Pos
import Lib.StdOutputs
import Buildsome.BuildId
import qualified Data.Map as Map

import Criterion.Main
import Criterion
import Criterion.Internal (runAndAnalyse)
import Criterion.Monad (withConfig)

seqId x = seq x x

runBenchmark db = do
    let serializedInteger = runPut (put (123456789 :: Integer))
        targetPath = "."
        target = Target [BS8.pack "output"] [BS8.pack "input"] [] (BS8.pack "cmd") (Pos.initialPos "pos")
        eLogVal = ExecutionLog
          { elBuildId = BuildId "build-id"
          , elCommand = BS8.pack "build-id"
          , elInputsDescs = Map.empty
          , elOutputsDescs = Map.empty
          , elStdoutputs = StdOutputs (BS8.pack "bla") (BS8.pack "bla")
          , elSelfTime = 0
          }
        eLog = seqId $ executionLog target db
    writeIRef eLog eLogVal
    withConfig defaultConfig $ do
      mapM_ (runAndAnalyse $ const True)
         [ bench "executionLog" $ whnfIO (readIRef eLog)
         , bench "getWord32le" $ nf (runGet getWord32le) $ seqId $ pack "1234"
         , bench "id" $ nf id $ seqId $ pack "1234"
         , bench "getWord32host" $ nf (runGet getWord32host) $ seqId $ pack "1234"
         , bench "get(Integer)" $ nf (runGet (get :: Get Integer)) $ seqId $ serializedInteger
         ]
    delIRef eLog



