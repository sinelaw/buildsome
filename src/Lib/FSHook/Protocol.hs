{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs, OverloadedStrings, DeriveGeneric, DeriveFunctor #-}
module Lib.FSHook.Protocol
  ( parseMsg, helloPrefix
  , OpenWriteMode(..), showOpenWriteMode
  , OpenTruncateMode(..), showOpenTruncateMode
  , CreationMode(..), showCreationMode
  , InFilePath, OutFilePath(..), OutEffect(..)
  , Func, FuncIO(..), showFunc
  , IsDelayed(..)
  , Msg(..)
  ) where

import Prelude.Compat hiding (FilePath)

import Data.Interned (Uninternable(..), intern)
import Data.Interned.ByteString (InternedByteString(..))
import Control.Monad
import Data.Binary.Get
import Data.Binary (Binary(..))
import GHC.Generics (Generic(..))
import Data.Bits
import Data.ByteString (ByteString)
import Data.IntMap (IntMap, (!))
import Data.Word
import Data.String (IsString(..))
import Lib.ByteString (truncateAt)
import Lib.Directory (catchDoesNotExist)
import Lib.FilePath (FilePath, (</>))
import Numeric (showOct)
import System.Posix.Files.ByteString (fileAccess)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap as M

data OpenWriteMode = WriteMode | ReadWriteMode
  deriving (Show, Generic)
instance Binary OpenWriteMode
showOpenWriteMode :: OpenWriteMode -> String
showOpenWriteMode WriteMode = ""
showOpenWriteMode ReadWriteMode = "+"
{-# INLINE showOpenWriteMode #-}

data CreationMode = NoCreate | Create !Word32 -- Unix permissions
  deriving (Show, Generic)
instance Binary CreationMode
showCreationMode :: CreationMode -> String
showCreationMode NoCreate = ""
showCreationMode (Create x) = " (CREATE:" ++ showOct x "" ++ ")"
{-# INLINE showCreationMode #-}

data OpenTruncateMode = OpenNoTruncate | OpenTruncate
  deriving (Show, Generic)
instance Binary OpenTruncateMode
showOpenTruncateMode :: OpenTruncateMode -> String
showOpenTruncateMode OpenNoTruncate = ""
showOpenTruncateMode OpenTruncate = " (TRUNCATE)"

type InFilePath = FilePath

-- WARNING: The order of these constructors must match
-- fs_override.c:enum out_effect (due to Enum instance)!
data OutEffect
  = OutEffectNothing
  | OutEffectCreated
  | OutEffectDeleted
  | OutEffectChanged
  | OutEffectUnknown
  deriving (Eq, Ord, Show, Enum, Generic)
instance Binary OutEffect

data OutFilePath = OutFilePath
  { outPath :: !FilePath
  , outEffect :: !OutEffect
  } deriving (Eq, Ord, Show, Generic)
instance Binary OutFilePath

data FuncIO ifp ouf
  = OpenR ifp
  | OpenW ouf OpenWriteMode CreationMode OpenTruncateMode
  | Stat ifp
  | LStat ifp
  | Creat ouf Word32
  | Rename ouf ouf
  | Unlink ouf
  | Access ifp Word32{- TODO: replace Int with AccessMode -}
  | OpenDir ifp
  | Truncate ouf Word64{- length -}
  | Chmod ouf Word32{-mode-}
  | ReadLink ifp
  | MkNod ouf Word32{-mode-} Word64{-dev-}
  | MkDir ouf Word32{-mode-}
  | RmDir ouf
  | SymLink ifp ouf
  | Link ouf ouf
  | Chown ouf Word32 Word32
  | Exec ifp
  | ExecP (Maybe FilePath) [FilePath]{-prior searched paths (that did not exist)-}
  | RealPath ifp
  deriving (Show, Functor)


type Func = FuncIO InFilePath OutFilePath

bimapFunc :: (i -> i') -> (o -> o') -> FuncIO i o -> FuncIO i' o'
bimapFunc f g func =
  case func of
  OpenR ifp -> OpenR (f ifp)
  Stat ifp -> Stat (f ifp)
  LStat ifp -> LStat (f ifp)
  Access ifp w -> Access (f ifp) w
  OpenDir ifp -> OpenDir (f ifp)
  ReadLink ifp -> ReadLink (f ifp)
  SymLink ifp ouf -> SymLink (f ifp) (g ouf)
  Exec ifp -> Exec (f ifp)
  RealPath ifp -> RealPath (f ifp)
  OpenW ouf ow ct ot -> OpenW (g ouf) ow ct ot
  Creat ouf w -> Creat (g ouf) w
  Rename ouf1 ouf2 -> Rename (g ouf1) (g ouf2)
  Unlink ouf -> Unlink (g ouf)
  Truncate ouf w -> Truncate (g ouf) w
  Chmod ouf w -> Chmod (g ouf) w
  MkNod ouf w1 w2 -> MkNod (g ouf) w1 w2
  MkDir ouf w -> MkDir (g ouf) w
  RmDir ouf -> RmDir (g ouf)
  Link ouf1 ouf2 -> Link (g ouf1) (g ouf2)
  Chown ouf w1 w2 -> Chown (g ouf) w1 w2
  ExecP mf fs -> ExecP mf fs

internFuncPaths :: FuncIO ByteString o -> FuncIO ByteString o
internFuncPaths func = bimapFunc (\x -> unintern (intern (BS8.copy x) :: InternedByteString)) id func

-- Hook is delayed waiting for handler to complete
data IsDelayed = Delayed | NotDelayed
  deriving (Show)

data Msg = Msg
  { msgIsDelayed :: !IsDelayed
  , msgFunc :: !Func
  }
  deriving (Show)

{-# INLINE showFunc #-}
showFunc :: Func -> String
showFunc (OpenR path) = "open:" ++ show path
showFunc (OpenW path wmode creation trunc) = "openW" ++ showOpenWriteMode wmode ++ ":" ++ show path ++ showCreationMode creation ++ showOpenTruncateMode trunc
showFunc (Stat path) = "stat:" ++ show path
showFunc (LStat path) = "lstat:" ++ show path
showFunc (Creat path perms) = concat ["create:", show path, " ", showOct perms ""]
showFunc (Rename old new) = concat ["rename:", show old, "->", show new]
showFunc (Unlink path) = concat ["unlink:", show path]
showFunc (Access path mode) = concat ["access:", show path, " ", show mode]
showFunc (OpenDir path) = concat ["openDir:", show path]
showFunc (Truncate path len) = concat ["truncate:", show path, " ", show len]
showFunc (Chmod path perms) = concat ["chmod:", show path, " ", showOct perms ""]
showFunc (ReadLink path) = concat ["readlink:", show path]
showFunc (MkNod path mode dev) = unwords ["mknod:", show path, showOct mode "", show dev]
showFunc (MkDir path mode) = unwords ["mkdir:", show path, showOct mode ""]
showFunc (RmDir path) = unwords ["rmdir:", show path]
showFunc (SymLink target linkpath) = unwords ["symlink:", show target, show linkpath]
showFunc (Link src dest) = unwords ["link:", show src, show dest]
showFunc (Chown path uid gid) = unwords ["chown:", show path, show uid, show gid]
showFunc (Exec path) = unwords ["exec:", show path]
showFunc (ExecP (Just path) attempted) = unwords ["execP:", show path, "searched:", show attempted]
showFunc (ExecP Nothing attempted) = unwords ["failedExecP:searched:", show attempted]
showFunc (RealPath path) = unwords ["realPath:", show path]

{-# ANN module ("HLint: ignore Use ++"::String) #-}
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

mAX_PATH :: Int
mAX_PATH = 256
mAX_PATH_ENV_VAR_LENGTH :: Int
mAX_PATH_ENV_VAR_LENGTH = 10*1024
mAX_PATH_CONF_STR :: Int
mAX_PATH_CONF_STR = 10*1024
mAX_EXEC_FILE :: Int
mAX_EXEC_FILE = mAX_PATH

getNullTerminated :: Int -> Get FilePath
getNullTerminated len = BS8.copy . truncateAt 0 <$> getByteString len

getPath :: Get FilePath
getPath = do
  path <- getNullTerminated mAX_PATH
  return $ (path :: ByteString)

getInPath :: Get InFilePath
getInPath = getPath

getOutEffect :: Get OutEffect
getOutEffect = toEnum . fromIntegral <$> getWord32le

getOutPath :: Get OutFilePath
getOutPath = OutFilePath <$> getPath <*> getOutEffect

fLAG_ALSO_READ :: Word32
fLAG_ALSO_READ = 1
fLAG_CREATE :: Word32
fLAG_CREATE = 2
fLAG_TRUNCATE :: Word32
fLAG_TRUNCATE = 4

{-# INLINE parseOpenW #-}
parseOpenW :: Get Func
parseOpenW = mkOpen <$> getOutPath <*> getWord32le <*> getWord32le
  where
    mkOpen path flags mode =
      OpenW path (openMode flags) (creationMode flags mode) (isTruncate flags)
    openMode flags
      | 0 /= flags .&. fLAG_ALSO_READ = ReadWriteMode
      | otherwise = WriteMode
    creationMode flags mode
      | 0 /= flags .&. fLAG_CREATE = Create mode
      | otherwise = NoCreate
    isTruncate flags
      | 0 /= flags .&. fLAG_TRUNCATE = OpenTruncate
      | otherwise = OpenNoTruncate

execP :: FilePath -> FilePath -> FilePath -> FilePath -> IO Func
execP file cwd envPath confStrPath
  | "/" `BS8.isInfixOf` file = return $ ExecP (Just file) []
  | otherwise = search [] allPaths
  where
    split = BS8.split ':'
    allPaths =
      map ((</> file) . (cwd </>)) $ split envPath ++ split confStrPath
    search attempted [] = return $ ExecP Nothing attempted
    search attempted (path:paths) = do
      canExec <- fileAccess path False False True `catchDoesNotExist` return False
      if canExec
        then return $ ExecP (Just path) attempted
        else search (path:attempted) paths

funcs :: IntMap (String, Get (IO Func))
funcs =
  M.fromList
  [ (0x10000, ("openR"   , return <$> (OpenR <$> getInPath)))
  , (0x10001, ("openW"   , return <$> parseOpenW)) -- TODO: Parse here, do post-process in func like execP
  , (0x10002, ("creat"   , return <$> (Creat <$> getOutPath <*> getWord32le)))
  , (0x10003, ("stat"    , return <$> (Stat <$> getInPath)))
  , (0x10004, ("lstat"   , return <$> (LStat <$> getInPath)))
  , (0x10005, ("opendir" , return <$> (OpenDir <$> getInPath)))
  , (0x10006, ("access"  , return <$> (Access <$> getInPath <*> getWord32le)))
  , (0x10007, ("truncate", return <$> (Truncate <$> getOutPath <*> getWord64le)))
  , (0x10008, ("unlink"  , return <$> (Unlink <$> getOutPath)))
  , (0x10009, ("rename"  , return <$> (Rename <$> getOutPath <*> getOutPath)))
  , (0x1000A, ("chmod"   , return <$> (Chmod <$> getOutPath <*> getWord32le)))
  , (0x1000B, ("readlink", return <$> (ReadLink <$> getInPath)))
  , (0x1000C, ("mknod"   , return <$> (MkNod <$> getOutPath <*> getWord32le <*> getWord64le)))
  , (0x1000D, ("mkdir"   , return <$> (MkDir <$> getOutPath <*> getWord32le)))
  , (0x1000E, ("rmdir"   , return <$> (RmDir <$> getOutPath)))
  , (0x1000F, ("symlink" , return <$> (SymLink <$> getInPath <*> getOutPath)))
  , (0x10010, ("link"    , return <$> (Link <$> getOutPath <*> getOutPath)))
  , (0x10011, ("chown"   , return <$> (Chown <$> getOutPath <*> getWord32le <*> getWord32le)))
  , (0x10012, ("exec"    , return <$> (Exec <$> getInPath)))
  , (0x10013, ("execp"   , execP <$> getNullTerminated mAX_EXEC_FILE <*> getPath <*> getNullTerminated mAX_PATH_ENV_VAR_LENGTH <*> getNullTerminated mAX_PATH_CONF_STR))
  , (0x10014, ("realPath", return <$> (RealPath <$> getInPath)))
  ]

{-# INLINE parseMsgLazy #-}
parseMsgLazy :: BSL.ByteString -> IO Msg
parseMsgLazy bs =
  Msg isDelayed <$> mkFunc
  where
    (isDelayed, mkFunc) = (`runGet` bs) $ do
      isDelayedInt <- getWord8
      funcId <- getWord32le
      let (_name, getter) = funcs ! fromIntegral funcId
      ioFunc <- getter
      finished <- isEmpty
      unless finished $ fail "Unexpected trailing input in message"
      return (if isDelayedInt == 0 then NotDelayed else Delayed, internFuncPaths <$> ioFunc)

{-# INLINE strictToLazy #-}
strictToLazy :: ByteString -> BSL.ByteString
strictToLazy x = BSL.fromChunks [x]

{-# INLINE parseMsg #-}
parseMsg :: ByteString -> IO Msg
parseMsg = parseMsgLazy . strictToLazy

{-# INLINE helloPrefix #-}
helloPrefix :: ByteString
helloPrefix = "PROTOCOL7: HELLO, I AM: "
