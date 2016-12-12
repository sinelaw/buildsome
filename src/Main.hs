{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
module Main (main) where

import qualified Buildsome.BuildMaps as BuildMaps
import qualified Buildsome.Color as Color
import           Buildsome.Opts (Opts(..), Opt(..))
import qualified Buildsome.Opts as Opts
import           Lib.SyncMap (SyncMap)
import qualified Lib.SyncMap as SyncMap
import           Lib.ByteString (unprefixed)
import           Lib.ColorText (ColorText)
import qualified Lib.ColorText as ColorText
import           Lib.Directory (getMFileStatus)
import           Lib.FilePath (FilePath, (</>))
import qualified Lib.FilePath as FilePath
import           Lib.Makefile (Makefile)
import qualified Lib.Makefile as Makefile
import           Lib.Printer (Printer)
import qualified Lib.Printer as Printer
import           Lib.ScanFileUpwards (scanFileUpwards)
import           Lib.Show (show)
import           Lib.TimeIt (timeIt)
import qualified Lib.Version as Version

import qualified BMake.User as BMake
import qualified Control.Exception as E
import           Control.Monad (forM_, when)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Functor.Identity (Identity(..))
import           Data.List (foldl')
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Monoid
import           Data.String (IsString(..))
import           Data.Typeable (Typeable)
import           GHC.Conc (setNumCapabilities, getNumProcessors)
import           System.Exit (exitWith, ExitCode(..))
import qualified System.IO as IO
import qualified System.IO.Error as Err
import qualified System.Posix.ByteString as Posix
import           System.Posix.IO (stdOutput)
import           System.Posix.Terminal (queryTerminal)
import qualified Data.Set as Set
import qualified Prelude.Compat as Prelude
import           Prelude.Compat hiding (FilePath, show)

standardMakeFilename :: FilePath
standardMakeFilename = "Buildsome.mk"

errorShowConcat :: (ColorText -> ByteString) -> [ColorText] -> [Char]
errorShowConcat render = BS8.unpack . render . cError . mconcat
    where
      Color.Scheme{..} = Color.scheme

data SpecifiedInexistentMakefilePath =
  SpecifiedInexistentMakefilePath (ColorText -> ByteString) FilePath
  deriving (Typeable)
instance Show SpecifiedInexistentMakefilePath where
  show (SpecifiedInexistentMakefilePath render path) =
    errorShowConcat render
    ["Specified makefile path: ", cPath (show path), " does not exist"]
    where
      Color.Scheme{..} = Color.scheme
instance E.Exception SpecifiedInexistentMakefilePath

specifiedMakefile :: Printer -> FilePath -> IO FilePath
specifiedMakefile printer path = do
  mStat <- getMFileStatus path
  case mStat of
    Nothing -> E.throwIO $ SpecifiedInexistentMakefilePath (Printer.render printer) path
    Just stat
      | Posix.isDirectory stat -> return $ path </> standardMakeFilename
      | otherwise -> return path

data BadCommandLine = BadCommandLine (ColorText -> ByteString) String deriving (Typeable)
instance E.Exception BadCommandLine
instance Show BadCommandLine where
  show (BadCommandLine render msg) =
    errorShowConcat render ["Invalid command line options: ", fromString msg]

setBuffering :: IO ()
setBuffering = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

switchDirectory :: FilePath -> IO (FilePath, FilePath, FilePath)
switchDirectory makefilePath = do
  origCwd <- Posix.getWorkingDirectory
  newCwd <-
    if BS8.null cwd then return origCwd
    else do
      Posix.changeWorkingDirectory cwd
      fullCwd <- FilePath.canonicalizePath $ origCwd </> cwd
      BS8.putStrLn $ "make: Entering directory `" <> fullCwd <> "'"
      return fullCwd
  return (origCwd, file, newCwd)
  where
    (cwd, file) = FilePath.splitFileName makefilePath

parseMakefile :: Printer -> FilePath -> FilePath -> Makefile.Vars -> FilePath -> IO Makefile
parseMakefile printer origMakefilePath finalMakefilePath vars cwd = do
  let absFinalMakefilePath = cwd </> finalMakefilePath
  (parseTime,  makefile) <- timeIt $ do
    rawMakefile <- BMake.parse absFinalMakefilePath vars
    let makefile = runIdentity $ Makefile.onMakefilePaths (Identity . FilePath.canonicalizePathAsRelativeCwd cwd) rawMakefile
    Makefile.verifyPhonies makefile
    return makefile
  -- let msg = "Parsed makefile: "
  -- Printer.rawPrintStrLn printer $ mconcat
  --   [ msg, cPath (show origMakefilePath)
  --  , " (took ", cTiming (show parseTime <> "sec"), ")"]
  return makefile
  where
    Color.Scheme{..} = Color.scheme

data MakefileScanFailed = MakefileScanFailed (ColorText -> ByteString) deriving (Typeable)
instance E.Exception MakefileScanFailed
instance Show MakefileScanFailed where
  show (MakefileScanFailed render) =
    errorShowConcat render
    [ "ERROR: Cannot find a file named "
    , cPath (show standardMakeFilename)
    , " in this directory or any of its parents"
    ]
    where
      Color.Scheme{..} = Color.scheme

-- TODO: Extract the --enable/disable colors outside of the
-- version/etc separation.
getColorRender :: Opts -> IO (ColorText -> ByteString)
getColorRender GetVersion = return ColorText.stripColors
getColorRender (Opts opt) =
  case optColor opt of
  Opts.ColorDisable -> return ColorText.stripColors
  Opts.ColorEnable -> return ColorText.render
  Opts.ColorDefault -> do
    isTty <- queryTerminal stdOutput
    return $
      if isTty
      then ColorText.render
      else ColorText.stripColors

flagPrefix :: ByteString
flagPrefix = "FLAG_"

optVars :: Opt -> Makefile.Vars
optVars opt = M.fromList $ withs ++ withouts
  where
    asVars val = map $ \name -> (flagPrefix <> name, val)
    withs = asVars "enable" $ optWiths opt
    withouts = asVars "disable" $ optWithouts opt

flagsOfVars :: Makefile.Vars -> Makefile.Vars
flagsOfVars = M.fromList . mapMaybe filterFlag . M.toList
  where
    filterFlag (key, value) =
      flip (,) value <$> unprefixed flagPrefix key

ljust :: Int -> ByteString -> ByteString
ljust padding bs = bs <> BS8.replicate (padding - l) ' '
  where
    l = BS8.length bs

showHelpFlags :: Makefile.Vars -> IO ()
showHelpFlags flags = do
  BS8.putStrLn "Available flags:"
  -- TODO: Colors (depends on --enable/disbale color being outside)
  let varNameWidth = 1 + (foldl' max 0 . map BS8.length . M.keys) flags
  forM_ (M.toList flags) $ \(varName, defaultVal) ->
    BS8.putStrLn $ mconcat
      ["  ", ljust varNameWidth varName, "(default = ", show defaultVal, ")"]

verifyValidFlags :: Makefile.Vars -> [ByteString] -> IO ()
verifyValidFlags validFlags userFlags
  | null invalidUserFlags = return ()
  | otherwise = fail $ "Given non-existent flags: " ++ show invalidUserFlags
  where
    invalidUserFlags = filter (`M.notMember` validFlags) userFlags

handleOpts ::
  Printer -> Opts -> (Makefile -> IO ()) -> IO ()
handleOpts printer GetVersion _ =
  Printer.rawPrintStrLn printer $ "buildsome " <> Version.version
handleOpts printer (Opts opt) body = do
  origMakefilePath <-
    case optMakefilePath opt of
    Nothing ->
      maybe (E.throwIO (MakefileScanFailed (Printer.render printer))) return =<<
      scanFileUpwards standardMakeFilename
    Just path -> specifiedMakefile printer path
  (_origCwd, finalMakefilePath, cwd) <- switchDirectory origMakefilePath
  makefile <- parseMakefile printer origMakefilePath finalMakefilePath (optVars opt) cwd
  let flags = flagsOfVars (Makefile.makefileWeakVars makefile)
  if optHelpFlags opt
    then showHelpFlags flags
    else do
      verifyValidFlags flags (optWiths opt ++ optWithouts opt)
      body makefile


-- Includes "fail" and "error" (i.e: userError is part of IOError)
-- Other error types are trusted to do their own color pretty-printing
ioErrorHandler :: Printer -> Err.IOError -> IO ()
ioErrorHandler printer err =
    do
        Printer.rawPrintStrLn printer $ cError $ show err
        exitWith (ExitFailure 1)
    where
        Color.Scheme{..} = Color.scheme

cachedBuildMapFind :: SyncMap FilePath (Maybe (Makefile.TargetKind, Makefile.TargetDesc)) -> BuildMaps.BuildMaps -> FilePath -> IO (Maybe (Makefile.TargetKind, Makefile.TargetDesc))
cachedBuildMapFind syncMap buildMaps filePath = do
  mHashes <- SyncMap.lookup syncMap filePath
  case mHashes of
      Nothing -> SyncMap.insert syncMap filePath updateCache
      Just hashes -> return hashes
  where
      updateCache = return $ BuildMaps.find buildMaps filePath
      -- do
      --     putStrLnStdErr (showLength $ filePath)
      --     res <- printTimeIt "BuildMaps.find" $ do
      --         let res' = BuildMaps.find buildMaps filePath
      --         putStrLnStdErr (showLength $ res')
      --         return res'
      --     return res

{-# INLINE andM #-}
andM :: Monad m => (a -> m Bool) -> [a] -> m Bool
andM check = go
    where
        go [] = return True
        go (x:xs) =
            do
                res <- check x
                if res
                    then go xs
                    else return False

data FileBuildRule
  = NoBuildRule
  | PhonyBuildRule
  | ValidBuildRule
  | InvalidPatternBuildRule {- transitively missing inputs -}
  deriving (Eq)

ptime :: t -> t1 -> t1
ptime _ y = y

-- getFileBuildRule :: BuildMaps -> Set FilePath -> Set FilePath -> FilePath -> IO FileBuildRule
getFileBuildRule :: SyncMap FilePath (Maybe (Makefile.TargetKind, Makefile.TargetDesc)) -> BuildMaps.BuildMaps -> FilePath -> IO FileBuildRule
getFileBuildRule syncMap buildMaps = go
  where
    registeredOutputs = Set.empty :: Set.Set FilePath
    phonies = Set.empty
    go path
      | path `Set.member` phonies = return PhonyBuildRule
      | otherwise = (ptime "cachedBuildMapFind" $ cachedBuildMapFind syncMap buildMaps path) >>= \res ->
        case res of
        Nothing -> return NoBuildRule
        Just (BuildMaps.TargetSimple, _) -> return ValidBuildRule
        Just (BuildMaps.TargetPattern, targetDesc) ->
          do
            inputsCanExist <- andM fileCanExist (Makefile.targetAllInputs (Makefile.tdTarget targetDesc))
            return $
              if inputsCanExist
              then ValidBuildRule
              else InvalidPatternBuildRule
    fileCanExist path = do
      fileBuildRule <- go path
      case fileBuildRule of
        InvalidPatternBuildRule -> return False
        PhonyBuildRule -> return False
        ValidBuildRule -> return True
        NoBuildRule
          | path `Set.member` registeredOutputs -> return False -- a has-been
          | otherwise -> ptime "FilePath.exists" $ FilePath.exists path


writeField :: ByteString -> IO ()
writeField bs = do
    let linesCount = length (BS8.split '\n' bs)
    -- putStrLnStdErr (show linesCount <> "\n" <> BS8.unpack bs)
    putStrLn $ show $ linesCount
    when (linesCount > 0) $ BS8.putStrLn bs

writeEmptyEntry :: IO ()
writeEmptyEntry = do
    writeField ""
    writeField ""
    writeField ""

checkEntry :: SyncMap FilePath (Maybe (Makefile.TargetKind, Makefile.TargetDesc)) -> BuildMaps.BuildMaps -> IO ()
checkEntry syncMap buildMaps  = do
    let go = do
            path <- BS8.pack <$> getLine
            -- putStrLnStdErr (BS8.unpack $ "(BUILDSOME) Query: '" <> path <> "'")
            buildRule <- ptime "getFileBuildRule" $ getFileBuildRule syncMap buildMaps path
            case buildRule of
                ValidBuildRule -> do
                    mTarget <- ptime "cachedBuildMapFind" $ cachedBuildMapFind syncMap buildMaps path
                    case mTarget of
                        Nothing -> writeEmptyEntry
                        Just (_targetKind, targetDesc) -> do
                            let target = Makefile.tdTarget targetDesc
                            case Makefile.targetCmds target of
                                Left bs -> writeField bs
                                Right _exprss -> error "what"
                            let inputs = BS8.intercalate "\n" (Makefile.targetInputs target)
                                outputs = BS8.intercalate "\n" (Makefile.targetOutputs target)
                            writeField inputs
                            writeField outputs
                _ -> writeEmptyEntry
            go
    go

main :: IO ()
main = do
  setBuffering
  setNumCapabilities =<< getNumProcessors
  opts <- Opts.get
  render <- getColorRender opts
  printer <- Printer.new render $ Printer.Id 0
  syncMap <- SyncMap.new
  E.handle (ioErrorHandler printer) $
    handleOpts printer opts $
    \makefile -> do
      --Print.buildsomeCreation printer Version.version optWiths optWithouts optVerbosity
      let buildMaps = BuildMaps.make makefile
      checkEntry syncMap buildMaps
