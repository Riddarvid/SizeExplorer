{-# LANGUAGE InstanceSigs #-}

module FileSystem (
  FileSystem (FS),
  Directory (..),
  File (File),
  FileError (..),
  Path (..),
  emptyPath,
  emptyFS,
  dirSize,
  fileSize,
  showDir,
  getDir,
  isLoaded,
  moveUp,
  moveDown
) where
import System.FilePath ((</>), splitDirectories, takeDirectory)
import Data.Foldable (find, foldlM)
import Utils (bytesToHuman)
import Text.Printf (printf)

newtype FileSystem = FS (Maybe (FilePath, Directory))

data Directory = Directory String Integer [Directory] [File] | BadDir String FileError
  deriving (Eq)

instance Ord Directory where
  compare :: Directory -> Directory -> Ordering
  compare (BadDir _ _) _ = LT
  compare _ (BadDir _ _) = GT
  compare (Directory name1 size1 _ _) (Directory name2 size2 _ _) =
    compareFileDir name1 size1 name2 size2


data File = File String Integer
  deriving (Eq)

instance Ord File where
  compare :: File -> File -> Ordering
  compare (File name1 size1) (File name2 size2) = compareFileDir name1 size1 name2 size2

data FileError = NoSuchFile | NoPermission | Other String
  deriving (Eq)

instance Show FileError where
  show :: FileError -> String
  show NoSuchFile = "File does not exist"
  show NoPermission = "You don't have permission to open that file"
  show (Other err) = err

compareFileDir :: String -> Integer -> String -> Integer -> Ordering
compareFileDir name1 size1 name2 size2 = case compare size1 size2 of
  EQ -> compare name1 name2
  res -> res

showDir :: Directory -> String
showDir = showDir' showDirBase

showDir' :: (Directory -> String) -> Directory -> String
showDir' show' dir@(Directory _ _ subdirs files) =
  showDirBase dir ++
  "\n\nFiles: \n" ++ files' ++
  "\nDirs: \n" ++ subdirs'
  where
    files' = unlines $ map showFile files
    subdirs' = unlines $ map show' subdirs
showDir' _ dir@(BadDir _ _) = showDirBase dir

showDirBase :: Directory -> String
showDirBase (Directory name size _ _) = showFileDir name size
showDirBase (BadDir name err) = showTab name (showErrorShort err)

showErrorShort :: FileError -> String
showErrorShort NoSuchFile = "No such file"
showErrorShort NoPermission = "Permission denied"
showErrorShort (Other _) = "Unknown error"

showFile :: File -> String
showFile (File name size) = showFileDir name size

showFileDir :: String -> Integer -> String
showFileDir name size = showTab name (bytesToHuman size)

showTab :: String -> String -> String
showTab = printf "%-30s%-10s"

emptyFS :: FileSystem
emptyFS = FS Nothing

getDirName :: Directory -> String
getDirName (Directory name _ _ _) = name
getDirName (BadDir name _) = name

dirSize :: Directory -> Integer
dirSize (Directory _ size _ _) = size
dirSize (BadDir _ _) = 0

fileSize :: File -> Integer
fileSize (File _ size) = size

isLoaded :: FileSystem -> Bool
isLoaded (FS Nothing) = False
isLoaded _ = True

-- Ide: Låt helt enkelt path vara en sträng och använd FilePath lib för att plocka ut dir.
newtype Path = Path FilePath

instance Show Path where
  show :: Path -> String
  show (Path path) = path

emptyPath :: Path
emptyPath = Path "."

getDir :: Directory -> Path -> Maybe Directory
getDir dir (Path path) = foldlM getSubDir dir path'
  where
    path' = splitDirectories path

getSubDir :: Directory -> String -> Maybe Directory
getSubDir dir "." = Just dir
getSubDir (Directory _ _ subDirs _) subDirName = find ((== subDirName) . getDirName) subDirs
getSubDir _ _ = Nothing

moveUp :: Path -> Path
moveUp (Path path) = Path $ takeDirectory path

moveDown :: Path -> String -> Path
moveDown (Path path) dir = Path $ path </> dir