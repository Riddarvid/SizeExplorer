{-# LANGUAGE GeneralizedNewtypeDeriving, InstanceSigs #-}

module Explorer (Explorer, runExplorer, analyzeDir, tryAnalyzeDir, showStatus, showCaret,
  loadFileSystem, ExplorerError(..), moveTo, moveToRoot) where

import FileSystem
  (FileSystem (FS), Directory (Directory, BadDir), File (File),
  emptyFS, dirSize, fileSize, FileError (NoSuchFile, NoPermission, Other),
  showDir, Path (Path), emptyPath, getDir, moveUp, moveDown)

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
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

data ExplorerState = ES {
  esPath :: Path,
  esFs :: FileSystem
}

emptyState :: ExplorerState
emptyState = ES {esPath = emptyPath, esFs = emptyFS}

data ExplorerError = CmdUsgErr String | FileErr FileError | FSNotLoaded

instance Show ExplorerError where
  show :: ExplorerError -> String
  show (CmdUsgErr msg) = msg
  show (FileErr err) = show err
  show FSNotLoaded = "No directory loaded"

-- Except ytterst - ändringar som orsakar ett fel har ingen påverkan på state.

newtype Explorer a = MKExplorer
  (ExceptT ExplorerError
    (StateT ExplorerState IO)
  a)
  deriving (Functor, Applicative,
             Monad, MonadState  ExplorerState
                  , MonadError  ExplorerError
                  , MonadIO
                  )

runExplorer :: Explorer a -> IO a
runExplorer (MKExplorer explorer) = do
  res <- evalStateT (runExceptT explorer) emptyState
  case res of
    Left err -> error $ show err
    Right res' -> return res'

-- File system

loadFileSystem :: FilePath -> Explorer ()
loadFileSystem path = do
  dir <- analyzeDir path
  let fs = FS (Just (path, dir))
  modify (\s -> s{esFs = fs, esPath = emptyPath})
  -- TODO: set correct path

tryAnalyzeDir :: FilePath -> Explorer Directory
tryAnalyzeDir path = catchError (analyzeDir path) badDir
  where
    badDir :: ExplorerError -> Explorer Directory
    badDir (FileErr err) = return $ BadDir (takeFileName path) err
    badDir _ = error "Should not happen."

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
    Left err -> throwError $ FileErr $ translateError err

translateError :: IOError -> FileError
translateError err
  | isPermissionError err = NoPermission
  | isDoesNotExistError err = NoSuchFile
  | otherwise = Other $ show err

-- REPL utils

fsRoot :: Explorer (FilePath, Directory)
fsRoot = do
  FS cont <- gets esFs
  case cont of
    Nothing -> throwError FSNotLoaded
    Just res -> return res

rootPath :: Explorer FilePath
rootPath = do
  (path, _) <- fsRoot
  return path

fullPath :: Explorer Path
fullPath = do
  root <- rootPath
  Path path <- gets esPath
  return $ Path (root </> path)

rootDir :: Explorer Directory
rootDir = do
  (_, dir) <- fsRoot
  return dir

showStatus :: Explorer String
showStatus = catchError showStatus' (return . show)

showStatus' :: Explorer String
showStatus' = do
  dir <- rootDir
  path <- gets esPath
  showFS dir path

showFS :: Directory -> Path -> Explorer String
showFS dir path = do
  dir' <- getValidDir dir path
  return $ showDir dir'

getValidDir :: Directory -> Path -> Explorer Directory
getValidDir dir path = case getDir dir path of
  Nothing -> throwError (FileErr NoSuchFile)
  Just (BadDir _ err) -> throwError (FileErr err)
  Just dir' -> return dir'

showCaret :: Explorer String
showCaret = do
  path <- catchError fullPath (\_ -> return $ Path "")
  return $ show path ++ "> "

-- CD

moveToRoot :: Explorer ()
moveToRoot = modify (\s -> s{esPath = emptyPath})

moveTo :: FilePath -> Explorer ()
moveTo dest = do
  path <- gets esPath
  let path' = if dest == ".." then moveUp path else moveDown path dest
  root <- trace (show path') rootDir
  case getDir root path' of
    Nothing -> throwError (FileErr NoSuchFile)
    Just _ -> modify (\s -> s{esPath = path'})