{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Buildsome.Db
  ( Db, with
  , registeredOutputsRef, leakedOutputsRef
  , InputDescWith(..)
  , InputDesc, inputDescDropReasons
  , OutputDesc(..)
  , ExecutionLog, ExecutionLogOf(..)
  , ExecutionLogNode(..)
  , executionLogNode, getExecutionLog
  , executionLogUpdate
  , executionLogLookup
  , latestExecutionLog
  , internString
  , FileDescInput, FileDescInputNoReasons
  , FileContentDescCache(..), fileContentDescCache
  , Reason(..)
  , IRef(..)
  , MFileContentDesc, MakefileParseCache(..), makefileParseCache
  ) where

import           Buildsome.BuildId (BuildId)
import qualified Lib.Hash as Hash
import           Lib.Hash (Hash)
import           Control.Monad (join)
import           Data.Binary (Binary(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Default (def)
import qualified Data.Either as Either
import           Data.IORef
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Time.Clock (DiffTime)
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Database.LevelDB.Base as LevelDB
import           GHC.Generics (Generic)
import           Lib.Binary (encode, decode)
import           Lib.Cmp (Cmp(..))
import qualified Lib.Cmp as Cmp
import           Lib.Directory (catchDoesNotExist, createDirectories, makeAbsolutePath)
import           Lib.Exception (bracket)
import qualified Lib.FSHook as FSHook
import           Lib.FileDesc (FileDesc(..), bimapFileDesc, FileContentDesc, FileModeDesc, FileStatDesc)
import           Lib.FilePath (FilePath, (</>), (<.>))
import           Lib.Makefile (Makefile)
import qualified Lib.Makefile as Makefile
import           Lib.Makefile.Monad (PutStrLn)

import           Lib.NonEmptyMap (NonEmptyMap)
import qualified Lib.NonEmptyMap as NonEmptyMap
import           Lib.StdOutputs (StdOutputs(..))
import           Lib.TimeInstances ()
import qualified System.Posix.ByteString as Posix
import           Control.Monad (forM)

import           Prelude.Compat hiding (FilePath)

schemaVersion :: ByteString
schemaVersion = "schema.ver.21"

#ifdef DEBUG_PRINT
debugPrint = putStrLn
#else
debugPrint x = return ()
#endif

data Db = Db
  { dbLevel :: LevelDB.DB
  , dbRegisteredOutputs :: IORef (Set FilePath)
  , dbLeakedOutputs :: IORef (Set FilePath)
  , dbStrings :: IORef (Map StringKey ByteString)
  }

data FileContentDescCache = FileContentDescCache
  { fcdcModificationTime :: POSIXTime
  , fcdcFileContentDesc :: FileContentDesc
  } deriving (Generic, Show)
instance Binary FileContentDescCache

data Reason
  = BecauseSpeculative Reason
  | BecauseHintFrom [FilePath]
  | BecauseHooked FSHook.AccessDoc
  | BecauseChildOfFullyRequestedDirectory Reason
  | BecauseContainerDirectoryOfInput Reason FilePath
  | BecauseContainerDirectoryOfOutput FilePath
  | BecauseInput Reason FilePath
  | BecauseRequested ByteString
  deriving (Generic, Show, Ord, Eq)
instance Binary Reason


data InputDescWith a = InputDescWith
  { idModeAccess :: Maybe (a, FileModeDesc)
  , idStatAccess :: Maybe (a, FileStatDesc)
  , idContentAccess :: Maybe (a, FileContentDesc)
  } deriving (Generic, Show, Ord, Eq, Functor)
instance Binary a => Binary (InputDescWith a)

type InputDesc = InputDescWith Reason

inputDescDropReasons :: InputDesc -> InputDescWith ()
inputDescDropReasons = fmap (const ())

data OutputDesc = OutputDesc
  { odStatDesc :: FileStatDesc
  , odContentDesc :: Maybe FileContentDesc -- Nothing if directory
  } deriving (Generic, Show, Eq)
instance Binary OutputDesc

-- TODO: naming...
type FileDescInput = FileDesc Reason (POSIXTime, InputDesc)
type FileDescInputNoReasons = FileDesc () (InputDescWith ())

fileDescInputDropReasons :: FileDescInput -> FileDescInputNoReasons
fileDescInputDropReasons = bimapFileDesc (const ()) (inputDescDropReasons . snd)

data ExecutionLogOf s = ExecutionLogOf
  { elBuildId :: BuildId
  , elCommand :: s
  , elInputsDescs :: [(s, FileDescInput)]
  , elOutputsDescs :: [(s, (FileDesc () (POSIXTime, OutputDesc)))]
  , elStdoutputs :: StdOutputs s
  , elSelfTime :: DiffTime
  } deriving (Generic, Functor, Foldable, Traversable)

type ExecutionLog = ExecutionLogOf ByteString
type ExecutionLogForDb = ExecutionLogOf StringKey
instance Binary (ExecutionLogOf StringKey)
instance Show (ExecutionLogOf ByteString)

newtype ExecutionLogNodeKey = ExecutionLogNodeKey Hash -- [(FilePath, FileDescInputNoReasons)]
  deriving (Generic, Show)
instance Binary ExecutionLogNodeKey

data ExecutionLogNode
  = ExecutionLogNodeBranch (NonEmptyMap StringKey (NonEmptyMap FileDescInputNoReasons ExecutionLogNodeKey))
  | ExecutionLogNodeLeaf ExecutionLogForDb
  deriving (Generic)
instance Binary ExecutionLogNode


registeredOutputsRef :: Db -> IORef (Set FilePath)
registeredOutputsRef = dbRegisteredOutputs

leakedOutputsRef :: Db -> IORef (Set FilePath)
leakedOutputsRef = dbLeakedOutputs

setKey :: Binary a => Db -> ByteString -> a -> IO ()
setKey db key val = LevelDB.put (dbLevel db) def key $ encode val

getKey :: Binary a => Db -> ByteString -> IO (Maybe a)
getKey db key = fmap decode <$> LevelDB.get (dbLevel db) def key

deleteKey :: Db -> ByteString -> IO ()
deleteKey db = LevelDB.delete (dbLevel db) def

options :: LevelDB.Options
options =
    LevelDB.defaultOptions
    { LevelDB.createIfMissing = True
    , LevelDB.errorIfExists = False
    , LevelDB.writeBufferSize = 64 * 1024 * 1024
    }

withLevelDb :: FilePath -> (LevelDB.DB -> IO a) -> IO a
withLevelDb dbPath =
  LevelDB.withDB (BS8.unpack (dbPath </> schemaVersion)) options

with :: FilePath -> (Db -> IO a) -> IO a
with rawDbPath body = do
  dbPath <- makeAbsolutePath rawDbPath
  createDirectories dbPath
  strings <- newIORef Map.empty
  withLevelDb dbPath $ \levelDb ->
    withIORefFile (dbPath </> "outputs") $ \registeredOutputs ->
    withIORefFile (dbPath </> "leaked_outputs") $ \leakedOutputs ->
    body (Db levelDb registeredOutputs leakedOutputs strings)
  where
    withIORefFile path =
      bracket (newIORef =<< decodeFileOrEmpty path) (writeBack path)
    writeBack path ref = do
      BS8.writeFile (BS8.unpack (path <.> "tmp")) .
        BS8.unlines . S.toList =<< readIORef ref
      Posix.rename (path <.> "tmp") path
    decodeFileOrEmpty path =
      (S.fromList . BS8.lines <$> BS8.readFile (BS8.unpack path))
      `catchDoesNotExist` return S.empty

data IRef a = IRef
  { readIRef :: IO (Maybe a)
  , writeIRef :: a -> IO ()
  , delIRef :: IO ()
  }

data TargetLogType
  = TargetLogLatestExecutionLog
  | TargetLogExecutionLogNode
  deriving (Show, Eq, Ord, Generic)
instance Binary TargetLogType

mkIRefKey :: (Binary a) => ByteString -> Db -> IRef a
mkIRefKey key db = IRef
  { readIRef = {-# SCC "readIRef" #-} getKey db key
  , writeIRef = {-# SCC "writeIRef" #-} setKey db key
  , delIRef = {-# SCC "deleteKey" #-} deleteKey db key
  }

newtype StringKey = StringKey { fromStringKey :: Hash }
  deriving (Generic, Show, Eq, Ord)
instance Binary StringKey

string :: StringKey -> Db -> IRef ByteString
string (StringKey k) = mkIRefKey $ "s:" <> Hash.asByteString k

updateString :: Db -> StringKey -> ByteString -> IO () -> IO ()
updateString db k s act = join $ atomicModifyIORef' (dbStrings db) $ \smap ->
  case Map.lookup k smap of
      Nothing -> (Map.insert k s smap, act)
      Just _ -> (smap, return ())

getString :: StringKey -> Db -> IO ByteString
getString k db = do
    smap <- readIORef $ dbStrings db
    case Map.lookup k smap of
        Nothing -> do
            s <- mustExist <$> readIRef (string k db)
            _ <- updateString db k s $ return ()
            return s
        Just s -> return s
    where
        mustExist Nothing = error $ "Corrupt DB? Missing string for key: " <> show k
        mustExist (Just s) = s

putString :: ByteString -> Db -> IO StringKey
putString s db = do
  _ <- updateString db k s $ writeIRef (string k db) s
  return k
  where
    k = StringKey (Hash.md5 s)

internString :: Db -> ByteString -> IO ByteString
internString db s = do
    putStrLn $ BS8.unpack $ "Interning: " <> s
    putString s db >>= flip getString db

-- TODO: Canonicalize commands (whitespace/etc)
targetKey :: TargetLogType -> Makefile.Target -> Hash
targetKey targetLogType target = Hash.md5 $ encode targetLogType <> Makefile.targetCmds target

executionLogNode :: ExecutionLogNodeKey -> Db -> IRef ExecutionLogNode
executionLogNode (ExecutionLogNodeKey k) = mkIRefKey $ "n:" <> Hash.asByteString k

executionLogLookup :: Makefile.Target -> Db -> (FilePath -> IO FileDescInputNoReasons) -> IO (Either (Maybe FilePath) ExecutionLog)
executionLogLookup target db getCurFileDesc = do
    let targetName =
            show $ case Makefile.targetOutputs target of
                   [] -> error "empty target?!"
                   (x:_) -> x
    debugPrint $ "executionLogLookup: looking up " <> targetName
    res <- executionLogLookup' (executionLogNode (executionLogNodeRootKey target) db) db getCurFileDesc
    let res' =
            case res of
            Left f -> "not found, missing: " <> show f
            Right _ -> "FOUND"
    debugPrint $ "executionLogLookup: " <> res' <> " - when looking up " <> targetName
    return res

maybeCmp :: Cmp a => Maybe a -> Maybe a -> Bool
maybeCmp (Just x) (Just y) = Cmp.Equals == (x `cmp` y)
maybeCmp _        _        = True

cmpFileDescInput :: FileDescInputNoReasons -> FileDescInputNoReasons -> Bool
cmpFileDescInput (FileDescExisting a) (FileDescExisting b)   =
  maybeCmp' (idModeAccess a) (idModeAccess b)
  && maybeCmp' (idStatAccess a) (idStatAccess b)
  && maybeCmp' (idContentAccess a) (idContentAccess b)
  where
    maybeCmp' x y = maybeCmp (snd <$> x) (snd <$> y)

cmpFileDescInput FileDescNonExisting{} FileDescNonExisting{} = True
cmpFileDescInput _                    _                      = False

getExecutionLog :: Db -> ExecutionLogForDb -> IO ExecutionLog
getExecutionLog db = traverse (flip getString db)

putExecutionLog :: Db -> ExecutionLog -> IO ExecutionLogForDb
putExecutionLog db = traverse (flip putString db)

executionLogLookup' :: IRef ExecutionLogNode -> Db -> (FilePath -> IO FileDescInputNoReasons) -> IO (Either (Maybe FilePath) ExecutionLog)
executionLogLookup' iref db getCurFileDesc = {-# SCC "executionLogLookup'" #-} do
    eln <- readIRef iref
    case eln of
        Nothing -> return $ Left Nothing
        Just (ExecutionLogNodeLeaf el) -> do
            debugPrint $ "executionLogLookup': found: " <> take 50 (show $ elCommand el)
            Right <$> getExecutionLog db el
        Just (ExecutionLogNodeBranch mapOfMaps) -> do
            logs <-
                forM (NonEmptyMap.toList mapOfMaps) $ \(filePathKey, mapOfFileDescs) -> do
                    filePath <- getString filePathKey db
                    curFileDesc <- getCurFileDesc filePath
                    let matchingKeys = map snd $ filter ((cmpFileDescInput curFileDesc) . fst) (NonEmptyMap.toList mapOfFileDescs)
                    case matchingKeys of
                        [] -> do
                            debugPrint $ "executionLogLookup': NO MATCH for " <> show filePath
                            return $ Left $ Just filePath
                        [k] -> do
                            debugPrint $ "executionLogLookup': traversing into " <> show filePath
                            executionLogLookup' (executionLogNode k db) db getCurFileDesc
                        _ -> error "waaat matchingKeys"
            let (lefts, rights) = Either.partitionEithers logs
            case rights of
                [] ->
                    case catMaybes lefts of
                        [] -> return $ Left Nothing
                        (fp:_) -> return $ Left $ Just fp
                (x:_) -> return $ Right x
                -- TODO
--                _ -> error "waaaaaat catMaybes logs"

executionLogNodeKey :: ExecutionLogNodeKey -> StringKey -> FileDescInputNoReasons -> ExecutionLogNodeKey
executionLogNodeKey (ExecutionLogNodeKey oldKey) sk fileDescInput = ExecutionLogNodeKey $ oldKey <> fromStringKey sk <> Hash.md5 (encode fileDescInput)

executionLogNodeRootKey :: Makefile.Target -> ExecutionLogNodeKey
executionLogNodeRootKey = ExecutionLogNodeKey . targetKey TargetLogExecutionLogNode

executionLogInsert :: Db -> ExecutionLogNodeKey -> ExecutionLog -> [(FilePath, FileDescInputNoReasons)] -> IO ExecutionLogNodeKey
executionLogInsert db key el inputsLeft = {-# SCC "executionLogInsert" #-} do
    let iref = executionLogNode key db
    debugPrint $ "executionLogInsert: inputsLeft: " <> (show $ length inputsLeft)
    case inputsLeft of
        [] -> do
            -- TODO check if exists at current iref and panic?
            eldb <- putExecutionLog db el
            writeIRef iref $ ExecutionLogNodeLeaf eldb
            return key
        (i@(inputFile, inputFileDesc):is) -> do
            inputFileKey <- putString inputFile db
            let nextKey = executionLogNodeKey key inputFileKey inputFileDesc
                mapOfMaps = NonEmptyMap.singleton inputFileKey (NonEmptyMap.singleton inputFileDesc nextKey)
            writeIRef iref $ ExecutionLogNodeBranch mapOfMaps
            executionLogInsert db nextKey el is

executionLogUpdate :: Makefile.Target -> Db -> ExecutionLog -> IO ExecutionLogNodeKey
executionLogUpdate target db el = executionLogUpdate' (executionLogNode key db) key db el inputFiles
    where
        key = executionLogNodeRootKey target
        inputFiles = map (\(f, d) -> (f, fileDescInputDropReasons d)) $ elInputsDescs el


executionLogUpdate' :: IRef ExecutionLogNode -> ExecutionLogNodeKey -> Db -> ExecutionLog
                       -> [(FilePath, FileDescInputNoReasons)] -> IO ExecutionLogNodeKey
executionLogUpdate' iref key db el [] = do
    -- TODO validate current iref key matches inputsPassed?
    eldb <- putExecutionLog db el
    writeIRef iref (ExecutionLogNodeLeaf eldb)
    return key
executionLogUpdate' iref key db el inputsLeft@(i@(inputFile, inputFileDesc):is) = {-# SCC "executionLogUpdate'_branch" #-} do
    eln <- readIRef iref
    inputFileKey <- putString inputFile db
    case eln of
        Nothing -> do
            let nextKey = executionLogNodeKey key inputFileKey inputFileDesc
                newMapOfMaps = NonEmptyMap.singleton inputFileKey (NonEmptyMap.singleton inputFileDesc nextKey)
            writeIRef iref (ExecutionLogNodeBranch newMapOfMaps)
            executionLogInsert db nextKey el inputsLeft
        Just ExecutionLogNodeLeaf{} -> error "wat" -- TODO
        Just (ExecutionLogNodeBranch mapOfMaps) -> do
            case NonEmptyMap.lookup inputFileKey mapOfMaps of
                Nothing -> do
                    let nextKey = executionLogNodeKey key inputFileKey inputFileDesc
                        updatedMapOfMaps = NonEmptyMap.insert inputFileKey (NonEmptyMap.singleton inputFileDesc nextKey) mapOfMaps
                    writeIRef iref (ExecutionLogNodeBranch updatedMapOfMaps)
                    executionLogInsert db nextKey el inputsLeft
                Just mapOfFileDescs -> do
                    let matchingKeys = map snd $ filter ((cmpFileDescInput inputFileDesc) . fst) (NonEmptyMap.toList mapOfFileDescs)
                    case matchingKeys of
                        [] -> do
                            let nextKey = executionLogNodeKey key inputFileKey inputFileDesc
                                updatedMapOfMaps = NonEmptyMap.insert inputFileKey (NonEmptyMap.insert inputFileDesc nextKey mapOfFileDescs) mapOfMaps
                            writeIRef iref (ExecutionLogNodeBranch updatedMapOfMaps)
                            executionLogInsert db nextKey el inputsLeft
                        [key] -> executionLogUpdate' (executionLogNode key db) key db el is
                        _ -> error "waaaat"

latestExecutionLog :: Makefile.Target -> Db -> IRef ExecutionLogNodeKey
latestExecutionLog = mkIRefKey . Hash.asByteString . targetKey TargetLogLatestExecutionLog

fileContentDescCache :: FilePath -> Db -> IRef FileContentDescCache
fileContentDescCache fp db = mkIRefKey ("c:" <> fp) db

type MFileContentDesc = FileDesc () FileContentDesc

data MakefileParseCache = MakefileParseCache
  { mpcInputs :: (FilePath, Map FilePath MFileContentDesc)
  , mpcOutput :: (Makefile, [PutStrLn])
  } deriving (Generic, Show)
instance Binary MakefileParseCache

makefileParseCache :: Db -> Makefile.Vars -> IRef MakefileParseCache
makefileParseCache db vars =
    mkIRefKey ("makefileParseCache_Schema.1:" <> Hash.asByteString (Hash.md5 $ encode vars)) db
