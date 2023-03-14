{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Explorer (Explorer, runExplorer, analyzeDir, tryAnalyzeDir, showStatus, showCaret,
  loadFileSystem) where

import FileSystem
  (FileSystem (FS), Directory (Directory, BadDir), File (File),
  emptyFS, dirSize, fileSize, FileError (NoSuchFile, NoPermission, Other),
  showDir, Path, emptyPath, getDir, showPath)

import Control.Monad.Error.Class ( MonadError (catchError, throwError) )
import Control.Monad.Except (ExceptT, runExceptT)

import Control.Monad.State.Class (MonadState, modify)
import Control.Monad.State ( StateT, evalStateT, gets )

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad ( filterM )

import System.Directory
  (listDirectory, doesDirectoryExist, doesFileExist, getFileSize)
import System.FilePath (takeFileName, (</>))
import System.IO.Error (tryIOError, isPermissionError, isDoesNotExistError)
import Data.List (sort, sortBy)
import Data.Ord (comparing)

data ExplorerState = ES {
  esPath :: Path,
  esFs :: FileSystem
}

emptyState :: ExplorerState
emptyState = ES {esPath = emptyPath, esFs = emptyFS}

newtype Explorer a = MKExplorer
  (ExceptT FileError
    (StateT ExplorerState IO)
  a)
  deriving (Functor, Applicative,
             Monad, MonadState  ExplorerState
                  , MonadError  FileError
                  , MonadIO
                  )

runExplorer :: Explorer a -> IO a
runExplorer (MKExplorer explorer) = do
  res <- evalStateT (runExceptT explorer) emptyState
  case res of
    Left err -> error $ "From run function: " ++ show err
    Right res' -> return res'

-- File system

loadFileSystem :: FilePath -> Explorer ()
loadFileSystem path = do
  dir <- analyzeDir path
  let fs = FS (Just dir)
  modify (\s -> s{esFs = fs})
  -- TODO: set correct path

tryAnalyzeDir :: FilePath -> Explorer Directory
tryAnalyzeDir path = catchError (analyzeDir path) badDir
  where
    badDir :: FileError -> Explorer Directory
    badDir err = return $ BadDir (takeFileName path) err

analyzeDir :: FilePath -> Explorer Directory
analyzeDir path = do
  contents <- liftIOError $ listDirectory path
  let contentPaths = map (path </>) contents

  subDirs <- liftIOError $ filterM doesDirectoryExist contentPaths
  subDirs' <- sortBy (flip compare) <$> mapM tryAnalyzeDir subDirs
  let subDirsSize = sum $ map dirSize subDirs'

  files <- liftIOError $ filterM doesFileExist contentPaths
  files' <- sortBy (flip compare) <$> mapM analyzeFile files
  let filesSize = sum $ map fileSize files'

  let name = takeFileName path
  let size = subDirsSize + filesSize
  return $ Directory name size subDirs' files'

-- TODO error handling
analyzeFile :: FilePath -> Explorer File
analyzeFile path = do
  let name = takeFileName path
  size <- liftIO $ getFileSize path
  return $ File name size

-- Error utils

liftIOError :: IO a -> Explorer a
liftIOError action = do
  res <- liftIO $ tryIOError action
  case res of
    Right res' -> return res'
    Left err -> throwError $ translateError err

translateError :: IOError -> FileError
translateError err
  | isPermissionError err = NoPermission
  | isDoesNotExistError err = NoSuchFile
  | otherwise = Other $ show err

-- REPL utils

showStatus :: Explorer String
showStatus = do
  fs <- gets esFs
  path <- gets esPath
  return $ showFS fs path

showFS :: FileSystem -> Path -> String
showFS (FS dir) path = case dir of
  Nothing -> "No directory loaded, use \"load path\""
  Just dir' -> showDir $ getDir dir' path

showCaret :: Explorer String
showCaret = do
  path <- gets esPath
  return $ showPath path ++ "> "
