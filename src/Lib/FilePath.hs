{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash, OverloadedStrings, UnboxedTuples #-}
module Lib.FilePath
  ( FilePath
  , isAbsolute, null, toString
  , splitFileName
  , canonicalizePath
  , canonicalizePathAsRelative
  , dropTrailingPathSeparator, addTrailingPathSeparator
  , (</>), (<.>)
  , takeDirectory, takeFileName
  , makeRelative, makeRelativeToCurrentDirectory
  , exists
  , toBS, fromBS
  ) where


import Prelude.Compat hiding (FilePath, null)
import Control.Monad (forM_)
import Control.Exception (catch, throwIO)
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Binary (Binary(..), Get)
import GHC.Read (Read(..))
import GHC.IO.Exception (IOErrorType(..))
import Lib.ByteString (unprefixed)
import System.IO.Error (ioeGetErrorType)
import qualified Data.ByteString.Char8 as BS8
import qualified System.Posix.ByteString as Posix
import qualified GHC.Prim as Prim
import qualified Data.Text.Array as A
import GHC.Base (Char(..), Int(..), writeCharArray#, indexCharArray#, newByteArray#)
import Data.String (IsString(..))
import GHC.ST (ST(..))
import Control.DeepSeq (NFData(..))
import Data.Char (ord, chr)
import Data.IORef (newIORef, IORef)
import System.IO.Unsafe (unsafePerformIO)

data FilePath = FilePath (IORef (), Integer) A.Array --Posix.RawFilePath

new :: Int -> ST s (A.MArray s)
new (I# len#) = ST $ \s1# ->
  case newByteArray# len# s1# of
    (# s2#, marr# #) -> (# s2#, A.MArray marr# #)

unsafeWriteChar :: A.MArray d -> Int -> Char -> ST d ()
unsafeWriteChar A.MArray{..} (I# i#) (C# e#) = ST $ \s1# ->
  case writeCharArray# maBA i# e# s1# of
    s2# -> (# s2#, () #)

unsafeIndex :: FilePath -> Int -> Char
unsafeIndex (FilePath _ A.Array{..}) (I# i#) = C# (indexCharArray# aBA i#)

safeIndex :: FilePath -> Int -> Maybe Char
safeIndex fp i = if null fp then Nothing else Just (unsafeIndex fp i)

fpHash :: FilePath -> Integer
fpHash fp = foldr accumHash  0 [0..fpLength fp - 1]
  where p = 256
        accumHash i s = s + (fromIntegral . ord $ unsafeIndex fp i) * (p ^ i)

setHash :: FilePath -> FilePath
setHash fp@(FilePath (r,_) ar) = FilePath (r, fpHash fp) ar

mkFilePath :: A.Array -> FilePath
mkFilePath = FilePath (unsafePerformIO $ newIORef (), 0)

instance Monoid FilePath where
  mempty = fromString []
  x `mappend` y = setHash . mkFilePath $ A.run $ do
    ar <- new (fpLength x + fpLength y)
    forM_ [0 .. fpLength x - 1]
      (\i -> unsafeWriteChar ar i $ unsafeIndex x i)
    forM_ [0 .. fpLength y - 1]
      (\i -> unsafeWriteChar ar (fpLength x + i) $ unsafeIndex y i)
    return ar

instance Show FilePath where
  show = show . toString

instance Eq FilePath where
  x@(FilePath xh _) == y@(FilePath yh _) =
    ((fpLength x) == (fpLength y))
    && (xh == yh)
    && compareRes
    where minLen = min (fpLength x) (fpLength y)
          compareAt i = (unsafeIndex x i) == (unsafeIndex y i)
          compareRes = foldr (\i p -> compareAt i && p) True [0 .. minLen - 1]


instance Ord FilePath where
  {-# INLINE compare #-}
  (FilePath (_,xh) _) `compare` (FilePath (_,yh) _) = xh `compare` yh


instance NFData FilePath where
  rnf (FilePath (_,h) A.Array{..}) = (rnf h, C# i) `seq` ()
    where i = indexCharArray# aBA 0#

instance Binary FilePath where
  get = fromString . reverse <$> go []
    where go s = do
            c <- get :: Get Char
            if ord c == 0
            then return s
            else go (c : s)
  put x = do
    mapM_ (put . unsafeIndex x) [0..fpLength x - 1]
    put (chr 0)

instance Read FilePath where
  readPrec = fromString <$> readPrec

instance IsString FilePath where
  fromString s = setHash . mkFilePath $ A.run $ do
    ar <- new (length s)
    forM_ (zip s [0..]) (\(c, i) -> unsafeWriteChar ar i c)
    return ar

null :: FilePath -> Bool
null fp = 0 == fpLength fp

fpLength :: FilePath -> Int
fpLength (FilePath _ ar) = I# (Prim.sizeofByteArray# (A.aBA ar))

fpInit :: FilePath -> FilePath
fpInit = fromString . init . toString

fpLast :: FilePath -> Maybe Char
fpLast fp = safeIndex fp (fpLength fp - 1)

isLast :: FilePath -> Char -> Bool
isLast fp c = fpLast fp == Just c

toString :: FilePath -> [Char]
toString fp = map (unsafeIndex fp) [0..fpLength fp - 1]

toBS :: FilePath -> ByteString
toBS = BS8.pack . toString

fromBS :: ByteString -> FilePath
fromBS = fromString . BS8.unpack

{-# INLINE exists #-}
exists :: FilePath -> IO Bool
exists path
  | null path = return True
  | otherwise = Posix.fileExist (toBS path) `catch`
    \e ->
    case ioeGetErrorType e of
    InappropriateType -> return False
    _ -> throwIO e

splitPath :: FilePath -> [FilePath]
splitPath fp = map fromBS $ splitPathToBS fp

splitPathToBS :: FilePath -> [ByteString]
splitPathToBS fp = if isAbsolute fp
                   then "/" : BS8.split '/' (BS8.tail (toBS fp))
                   else BS8.split '/' (toBS fp)

joinPath :: [FilePath] -> FilePath
joinPath = fromBS . joinPathBS . map toBS

joinPathBS :: [ByteString] -> ByteString
joinPathBS (path:paths) | path == "/" = "/" <> BS8.intercalate "/" paths
joinPathBS       paths                =        BS8.intercalate "/" paths

onFst :: (a -> a') -> (a, b) -> (a', b)
onFst f (x, y) = (f x, y)

splitFileName :: FilePath -> (FilePath, FilePath)
splitFileName = (\(x,y) -> (fromBS x, fromBS y)) . splitFileNameToBS

splitFileNameToBS :: FilePath -> (ByteString, ByteString)
splitFileNameToBS = onFst joinPathBS . f . splitPathToBS
  where
    f [] = ([], "")
    f [x] = ([], x)
    f (x:xs) = onFst (x:) $ f xs

takeDirectory :: FilePath -> FilePath
takeDirectory = fromBS . takeDirectoryToBS

takeDirectoryToBS :: FilePath -> ByteString
takeDirectoryToBS = joinPathBS . reverse . drop 1 . reverse . splitPathToBS

takeFileName :: FilePath -> FilePath
takeFileName path =
  case splitPath path of
  [] -> ""
  xs -> last xs

removeRedundantComponents :: FilePath -> FilePath
removeRedundantComponents =
  joinPath .
  foldr step [] .
  filter (/= ".") .
  splitPath
  where
    step "/" xs = "/" : xs
    step ".." xs = ".." : xs
    step _ ("..":xs) = xs
    step x xs = x:xs

dropTrailingPathSeparator :: FilePath -> FilePath
dropTrailingPathSeparator x
  | isLast x '/' = fpInit x
  | otherwise = x

dropTrailingPathSeparatorBS :: ByteString -> ByteString
dropTrailingPathSeparatorBS x
  | "/" `BS8.isSuffixOf` x = BS8.init x
  | otherwise = x


addTrailingPathSeparator :: FilePath -> FilePath
addTrailingPathSeparator x
  | isLast x '/' = x
  | otherwise = x <> "/"

(</>) :: FilePath -> FilePath -> FilePath
"." </> y = y
"" </> y = y
x </> y
  | isAbsolute y = y
  | otherwise = dropTrailingPathSeparator x <> "/" <> y

(<.>) :: FilePath -> FilePath -> FilePath
f <.> g = f <> "." <> g

isAbsolute :: FilePath -> Bool
isAbsolute fp = Just '/' == safeIndex fp 0

makeRelative :: FilePath -> FilePath -> FilePath
makeRelative prefix full =
  case unprefixed prefix' (toBS full') of
  Nothing -> full
  Just suffix -> fromBS $ fromMaybe suffix $ unprefixed "/" suffix
  where
    prefix' = dropTrailingPathSeparatorBS $ toBS prefix
    full' = dropTrailingPathSeparator full

makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory path = ((`makeRelative` path) . fromBS) <$> Posix.getWorkingDirectory

canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path = do
  curDir <- fromBS <$> Posix.getWorkingDirectory
  return $ dropTrailingPathSeparator $ removeRedundantComponents $ curDir </> path

canonicalizePathAsRelative :: FilePath -> IO FilePath
canonicalizePathAsRelative path = makeRelativeToCurrentDirectory =<< canonicalizePath path
