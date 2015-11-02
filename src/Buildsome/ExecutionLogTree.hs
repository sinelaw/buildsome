{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase #-}
module Buildsome.ExecutionLogTree
  ( lookup
  , append
  , fromExecutionLog
  )
  where

import           Buildsome.Db            (ExecutionLog(..),
                                          ExecutionLogTree(..),
                                          InputDesc(..), executionLogTreeNode,
                                          FileDescInput)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Lib.Cmp (Cmp(..), ComparisonResult(..), Reasons)
import qualified Lib.Directory as Dir
import           Lib.FileDesc            (FileDesc(..),
                                          fileContentDescOfStat,
                                          fileModeDescOfStat,
                                          fileStatDescOfStat,
                                          fileStatDescOfStat)
import           Lib.FilePath (FilePath)
import qualified Lib.NonEmptyMap as NonEmptyMap
import qualified Lib.Trie as Trie
import           Prelude.Compat hiding (FilePath, lookup)
import qualified System.Posix.ByteString as Posix

data MismatchReason
  = MismatchExpectedExisting
  | MismatchExpectedNonExisting
  | MismatchFileDiffers Reasons
  deriving (Show)

andRight :: [Either a t] -> Either a ()
andRight [] = Right ()
andRight (Left x : _) = Left x
andRight (Right _ : xs) = andRight xs

matchesCurrentFS ::
  (Functor m, Applicative m, MonadIO m) =>
  FilePath -> Maybe Posix.FileStatus -> FileDescInput ->
  m (Either [MismatchReason] ())
matchesCurrentFS filePath mStat inputDesc =
  case (inputDesc, mStat) of
  (FileDescNonExisting{}, Nothing) -> return $ Right ()
  (FileDescNonExisting{}, Just _ ) -> return $ Left [MismatchExpectedNonExisting]
  (FileDescExisting{}   , Nothing) -> return $ Left [MismatchExpectedExisting]
  (FileDescExisting (_mtime, inputFileDesc), Just stat) ->
    andRight <$> sequenceA
    [ compareProp (idModeAccess inputFileDesc)    (return $ fileModeDescOfStat stat)
    , compareProp (idStatAccess inputFileDesc)    (return $ fileStatDescOfStat stat)
    , compareProp (idContentAccess inputFileDesc) (liftIO $ fileContentDescOfStat filePath stat)
    ]

compareProp ::
  (Monad m, Cmp a) =>
  Maybe (a1, a) -> m a -> m (Either [MismatchReason] ())
compareProp prop act =
  maybe (return $ Right ()) check $ fmap snd prop
    where
      check x = do
        res <- act
        case x `cmp` res of
          Equals -> return (Right ())
          NotEquals r -> return (Left [MismatchFileDiffers r])

data ExecutionLogTreeRef m
  = ExecutionLogTreeRef { unELTRef :: ExecutionLogTree (m (ExecutionLogTreeRef m)) }

lookup :: MonadIO m => ExecutionLogTreeRef m -> m (Either [(FilePath, MismatchReason)] ExecutionLog)
lookup = Trie.lookup (executionLogNode . unELTRef) (liftIO . Dir.getMFileStatus) matchesCurrentFS

inputsList :: ExecutionLog -> [(FilePath, FileDescInput)]
inputsList ExecutionLog{..} = Map.toList elInputsDescs

fromExecutionLog :: Monad m => ExecutionLog -> ExecutionLogTreeRef m
fromExecutionLog el = fromExecutionLog' el $ inputsList el

fromExecutionLog' :: Monad m => ExecutionLog -> [(FilePath, FileDescInput)] -> ExecutionLogTreeRef m
fromExecutionLog' el =
    ExecutionLogTreeRef. ExecutionLogTree . \case
    [] -> Trie.Leaf el
    ((filePath, inputDesc) : inputs) ->
      Trie.Branch
      . NonEmptyMap.singleton filePath
      $ NonEmptyMap.singleton inputDesc (return $ fromExecutionLog' el inputs)

append :: Monad m => ExecutionLog -> ExecutionLogTreeRef m -> ExecutionLogTreeRef m
append el =
  ExecutionLogTreeRef . ExecutionLogTree
  . go (inputsList el, el) []
  . executionLogNode . unELTRef
  where
--    go :: (
    go ([], new) _ Trie.Leaf{} = Trie.Leaf new
    go ((filePath,_):_, _) oldInputs Trie.Leaf{}
      = error $ intercalate "\n\t"
        [ "Existing execution log with no further inputs exists, but new one has more inputs!"
        , "Target: ", show $ elOutputsDescs el
        , "Target command: ", show $ elCommand el
        , "Example extra input: ", show filePath
        , "Existing entry's inputs: ", concatMap (("\n\t\t" ++) . show) oldInputs
        ]
    go ([], _) _ Trie.Branch{}
      = error "Existing execution log has more inputs, but new one has no further inputs!"
    go ((filePath, inputDesc) : is, new) oldInputs (Trie.Branch inputses)
      = case NonEmptyMap.lookup filePath inputses of
          -- no filepath matching this input at current level
          Nothing -> Trie.Branch $ NonEmptyMap.insert filePath newInput inputses
            where
              newInput = NonEmptyMap.singleton inputDesc $ fromExecutionLog' new is

          -- found an input with the same file path, need to check
          -- it's filedesc
          Just branches ->
            case NonEmptyMap.lookup inputDesc branches of
              -- none of the existing filedescs matches what we have
              Nothing -> Trie.Branch $ NonEmptyMap.insert filePath newInput inputses
                where
                  newInput =
                    NonEmptyMap.insert inputDesc (return . fromExecutionLog' new is) branches

              -- found exact match, go down the tree
              Just next -> go (is, new) (filePath:oldInputs) (executionLogNode $ unELTRef next)

