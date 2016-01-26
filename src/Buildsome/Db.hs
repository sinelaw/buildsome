{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  , FileDescInput
  , FileContentDescCache(..), fileContentDescCache, bimapFileDesc
  , Reason(..)
  , IRef(..)
  , MFileContentDesc, MakefileParseCache(..), makefileParseCache
  ) where

import           Buildsome.BuildId (BuildId)
import           Control.Applicative ((<|>), Alternative(..))
import           Control.Monad (join, liftM, forM)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Either (runEitherT, EitherT(..), left)
import           Data.Binary (Binary(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Default (def)
import qualified Data.List as List

import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map

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
import           Lib.Hash (Hash)
import qualified Lib.Hash as Hash
import           Lib.Makefile (Makefile)
import qualified Lib.Makefile as Makefile
import           Lib.Makefile.Monad (PutStrLn)

import           Lib.StdOutputs (StdOutputs(..))



import           Lib.TimeInstances ()
import qualified System.Posix.ByteString as Posix


import           Prelude.Compat hiding (FilePath)

schemaVersion :: ByteString
schemaVersion = "schema.ver.21"

debugPrint :: MonadIO io => String -> io ()
#ifdef DEBUG_PRINT
debugPrint = liftIO . putStrLn
#else
debugPrint _ = liftIO $ return ()
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

data ReasonOf a
  = BecauseSpeculative (ReasonOf a)
  | BecauseHintFrom [a]
  | BecauseHooked FSHook.AccessDoc
  | BecauseChildOfFullyRequestedDirectory (ReasonOf a)
  | BecauseContainerDirectoryOfInput (ReasonOf a) a
  | BecauseContainerDirectoryOfOutput a
  | BecauseInput (ReasonOf a) a
  | BecauseRequested ByteString
  deriving (Generic, Show, Ord, Eq, Functor, Foldable, Traversable)
instance Binary a => Binary (ReasonOf a)

type Reason = ReasonOf FilePath

data InputDescWith a = InputDescWith
  { idModeAccess :: Maybe (a, FileModeDesc)
  , idStatAccess :: Maybe (a, FileStatDesc)
  , idContentAccess :: Maybe (a, FileContentDesc)
  } deriving (Generic, Show, Ord, Eq, Functor, Foldable, Traversable)
instance Binary a => Binary (InputDescWith a)

type InputDesc = InputDescWith (ReasonOf FilePath)

inputDescDropReasons :: InputDesc -> InputDescWith ()
inputDescDropReasons = fmap (const ())

data OutputDesc = OutputDesc
  { odStatDesc :: FileStatDesc
  , odContentDesc :: Maybe FileContentDesc -- Nothing if directory
  } deriving (Generic, Show, Eq)
instance Binary OutputDesc

-- TODO: naming...
data FileDescInputOf a
    = FileDescInputOfNonExisting (ReasonOf a)
    | FileDescInputOfExisting (POSIXTime, InputDescWith (ReasonOf a))
    deriving (Show, Generic, Functor, Foldable, Traversable)
instance Binary a => Binary (FileDescInputOf a)
type FileDescInput = FileDescInputOf FilePath

newtype ELBranchPath a = ELBranchPath { unELBranchPath :: [(a, FileDescInputOf a)] }
    deriving (Show, Generic, Functor, Foldable, Traversable)
instance Binary a => Binary (ELBranchPath a)

splitBranchPathAt :: Int -> ELBranchPath a -> (ELBranchPath a, ELBranchPath a)
splitBranchPathAt x (ELBranchPath p) = (ELBranchPath prefix, ELBranchPath suffix)
    where (prefix, suffix) = List.splitAt x p

data ExecutionLogOf s = ExecutionLogOf
  { elBuildId :: BuildId
  , elCommand :: s
  , elInputsDescs :: ELBranchPath s
  , elOutputsDescs :: [(s, (FileDesc () (POSIXTime, OutputDesc)))]
  , elStdoutputs :: StdOutputs s
  , elSelfTime :: DiffTime
  } deriving (Generic, Functor, Foldable, Traversable)

type ExecutionLog = ExecutionLogOf ByteString
type ExecutionLogForDb = ExecutionLogOf StringKey
instance Binary (ExecutionLogOf StringKey)
deriving instance Show (ExecutionLogOf ByteString)

newtype ExecutionLogNodeKey = ExecutionLogNodeKey Hash -- [(FilePath, FileDescInputNoReasons)]
  deriving (Generic, Show)
instance Binary ExecutionLogNodeKey

data ExecutionLogNode
  = ExecutionLogNodeBranch [(ELBranchPath StringKey, ExecutionLogNodeKey)]
  | ExecutionLogNodeLeaf ExecutionLogForDb
  deriving (Generic)
instance Binary ExecutionLogNode


registeredOutputsRef :: Db -> IORef (Set FilePath)
registeredOutputsRef = dbRegisteredOutputs

leakedOutputsRef :: Db -> IORef (Set FilePath)
leakedOutputsRef = dbLeakedOutputs

setKey :: Binary a => Db -> ByteString -> a -> IO ()
setKey db key val = {-# SCC "setKey" #-} LevelDB.put (dbLevel db) def key $ {-# SCC "setKey.encode" #-} encode val

getKey :: Binary a => Db -> ByteString -> IO (Maybe a)
getKey db key = {-# SCC "getKey" #-} fmap decode <$> {-# SCC "getKey.get" #-} LevelDB.get (dbLevel db) def key

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

getString :: Db -> StringKey -> IO ByteString
getString db k = {-# SCC "getString" #-} do
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

putString :: Db -> ByteString -> IO StringKey
putString db s = {-# SCC "putString" #-} do
  _ <- updateString db k s $ writeIRef (string k db) s
  return k
  where
    k = StringKey (Hash.md5 s)

-- TODO: Canonicalize commands (whitespace/etc)
targetKey :: TargetLogType -> Makefile.Target -> Hash
targetKey targetLogType target = Hash.md5 $ encode targetLogType <> Makefile.targetCmds target

executionLogNode :: ExecutionLogNodeKey -> Db -> IRef ExecutionLogNode
executionLogNode (ExecutionLogNodeKey k) = mkIRefKey $ "n:" <> Hash.asByteString k

executionLogLookup :: Makefile.Target -> Db -> ([(FilePath, FileDescInput)] -> EitherT e IO ()) -> (FilePath -> IO FileDescInput) -> IO (Either (Maybe FilePath) ExecutionLog)
executionLogLookup target db prepareInputs getCurFileDesc = {-# SCC "executionLogLookup" #-} do
    let targetName =
            show $ case Makefile.targetOutputs target of
                   [] -> error "empty target?!"
                   (x:_) -> x
    debugPrint $ "executionLogLookup: looking up " <> targetName
    res <- runEitherT $ executionLogLookup' (executionLogNode (executionLogNodeRootKey target) db) db prepareInputs getCurFileDesc
    case res of
        Left f -> debugPrint $ "not found, missing: " <> show f
        Right _ -> debugPrint "FOUND"
    return res

maybeCmp :: Cmp a => Maybe a -> Maybe a -> Bool
maybeCmp (Just x) (Just y) = Cmp.Equals == (x `cmp` y)
maybeCmp _        _        = True

cmpFileDescInput :: FileDescInputOf r -> FileDescInputOf r -> Bool
cmpFileDescInput (FileDescInputOfExisting (_, a)) (FileDescInputOfExisting (_, b))   =
  maybeCmp' (idModeAccess a) (idModeAccess b)
  && maybeCmp' (idStatAccess a) (idStatAccess b)
  && maybeCmp' (idContentAccess a) (idContentAccess b)
  where
    maybeCmp' x y = maybeCmp (snd <$> x) (snd <$> y)

cmpFileDescInput FileDescInputOfNonExisting{} FileDescInputOfNonExisting{} = True
cmpFileDescInput _                            _                            = False

getExecutionLog :: Db -> ExecutionLogForDb -> IO ExecutionLog
getExecutionLog = traverse . getString

putExecutionLog :: Db -> ExecutionLog -> IO ExecutionLogForDb
putExecutionLog = traverse . putString

allRight :: Monad m => [m (Either e a)] -> EitherT e m ()
allRight = foldr (>>) (return ()) . map EitherT

-- like <|> for Either where lefts are not monoids, but alternatives.
firstRightAlt :: (Monad m, Alternative f) => EitherT (f e) m a -> EitherT (f e) m a -> EitherT (f e) m a
firstRightAlt (EitherT m) (EitherT n) =
    EitherT $ m >>= \case
    Left l -> flip liftM n $
        \case
            Left l' -> Left (l <|> l')
            Right r -> Right r
    Right r -> return (Right r)

firstRight :: (Monad m, Foldable t) => t (EitherT (Maybe e) m a) -> EitherT (Maybe e) m a
firstRight = foldr firstRightAlt (left Nothing)

executionLogPathCheck ::
    (MonadIO m) =>
    Db -> (ByteString -> IO FileDescInput) -> ELBranchPath FilePath
    -> EitherT (Maybe FilePath) m ()
executionLogPathCheck db getCurFileDesc (ELBranchPath path) = {-# SCC "executionLogPathCheck" #-}
    allRight $ flip map path $ \(filePath, expectedFileDesc) -> do
        curFileDesc <- liftIO $ getCurFileDesc filePath
        if cmpFileDescInput curFileDesc expectedFileDesc
            then return $ Right ()
            else return $ Left (Just filePath)

executionLogPathCmp :: ELBranchPath StringKey -> ELBranchPath StringKey -> Bool
executionLogPathCmp (ELBranchPath x) (ELBranchPath y) =
    all (\(xf, yf) -> (fst xf == fst yf) && cmpFileDescInput (snd xf) (snd yf)) $ zip x y

pathChunkSize :: Int
pathChunkSize = 50

traverseFileDescInputReason act desc =
    case desc of
        FileDescNonExisting reason -> FileDescNonExisting <$> (traverse act reason)
        FileDescExisting (t, e) -> FileDescExisting . (t,) <$> ((traverse . traverse $ act) e)

getPathStrings :: Db -> ELBranchPath StringKey -> IO (ELBranchPath FilePath)
getPathStrings db = traverse (getString db)

putPathStrings :: Db -> ELBranchPath FilePath -> IO (ELBranchPath StringKey)
putPathStrings db = traverse (putString db)

executionLogLookup' ::
    IRef ExecutionLogNode -> Db ->
    ([(FilePath, FileDescInput)] -> EitherT e IO ()) ->
    (FilePath -> IO FileDescInput) ->
    EitherT (Maybe FilePath) IO ExecutionLog
executionLogLookup' iref db prepareInputs getCurFileDesc = {-# SCC "executionLogLookup'" #-} do
    eln <- liftIO $ readIRef iref
    case eln of
        Nothing -> left Nothing
        Just (ExecutionLogNodeLeaf el) -> do
            debugPrint $ "executionLogLookup': found: " <> take 50 (show $ elCommand el)
            liftIO $ getExecutionLog db el
        Just (ExecutionLogNodeBranch mapOfMaps) -> do
            resolvedPaths <- liftIO $ mapM (\(p, t) -> (,t) <$> getPathStrings db p) mapOfMaps
            buildInputsRes <- liftIO $ runEitherT $ do
                prepareInputs $ concatMap (unELBranchPath . fst) resolvedPaths
            case buildInputsRes of
                Right _ -> return ()
                Left filePath -> left Nothing
            mTarget <-
                runEitherT $ firstRight
                $ flip map resolvedPaths $ \(path, t) ->
                    (executionLogPathCheck db getCurFileDesc path >> pure t)
            case mTarget of
                Right target -> executionLogLookup' (executionLogNode target db) db prepareInputs getCurFileDesc
                Left mFilePath -> left mFilePath

executionLogNodeKey :: ExecutionLogNodeKey -> ELBranchPath StringKey -> ExecutionLogNodeKey
executionLogNodeKey (ExecutionLogNodeKey oldKey) (ELBranchPath is) =
    ExecutionLogNodeKey $ oldKey <> mconcat (map (\(sk, x) -> fromStringKey sk <> Hash.md5 (encode x)) is)

executionLogNodeRootKey :: Makefile.Target -> ExecutionLogNodeKey
executionLogNodeRootKey = ExecutionLogNodeKey . targetKey TargetLogExecutionLogNode

executionLogInsert :: Db -> ExecutionLogNodeKey -> ExecutionLog -> ELBranchPath StringKey -> IO ExecutionLogNodeKey
executionLogInsert db key el path = {-# SCC "executionLogInsert" #-} do
    debugPrint $ "executionLogInsert: inputsLeft: " <> (show $ length $ unELBranchPath path)
    let (prefix, suffix) = splitBranchPathAt pathChunkSize path
    case unELBranchPath prefix of
        [] -> do
            eldb <- putExecutionLog db el
            writeIRef (executionLogNode key db) $ ExecutionLogNodeLeaf eldb
            return key
        _  -> do
            let k = executionLogNodeKey key prefix
            leafKey <- executionLogInsert db k el suffix
            writeIRef (executionLogNode key db) $ ExecutionLogNodeBranch [(prefix, k)]
            return leafKey

executionLogUpdate :: Makefile.Target -> Db -> ExecutionLog -> IO ExecutionLogNodeKey
executionLogUpdate target db el = do
    path <- putPathStrings db $ elInputsDescs el
    executionLogUpdate' (executionLogNode key db) key db el path
    where
        key = executionLogNodeRootKey target


executionLogUpdate' :: IRef ExecutionLogNode -> ExecutionLogNodeKey -> Db -> ExecutionLog
                       -> ELBranchPath StringKey -> IO ExecutionLogNodeKey
executionLogUpdate' iref key db el (ELBranchPath []) = do
    -- TODO validate current iref key matches inputsPassed?
    eldb <- putExecutionLog db el
    writeIRef iref (ExecutionLogNodeLeaf eldb)
    return key
executionLogUpdate' iref key db el inputsLeft = {-# SCC "executionLogUpdate'_branch" #-} do
    eln <- readIRef iref
    case eln of
        Nothing -> executionLogInsert db key el inputsLeft
        Just ExecutionLogNodeLeaf{} -> error "wat? Got leaf when more inputs to add..." -- TODO
        Just (ExecutionLogNodeBranch mapOfMaps) -> do
            let (prefix, suffix) = splitBranchPathAt pathChunkSize inputsLeft
            case filter (executionLogPathCmp prefix . fst) mapOfMaps of
                [] -> do
                    let nextKey = executionLogNodeKey key prefix
                    finalKey <- executionLogInsert db nextKey el suffix
                    writeIRef iref (ExecutionLogNodeBranch $ (prefix, nextKey) : mapOfMaps)
                    return finalKey
                [(_, leafKey)] ->
                    executionLogUpdate' (executionLogNode leafKey db) leafKey db el suffix

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
