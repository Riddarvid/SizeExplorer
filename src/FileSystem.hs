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
  showPath
) where
import System.FilePath ((</>))
import Data.Foldable (find)
import Utils (bytesToHuman)
import Text.Printf (printf)

newtype FileSystem = FS (Maybe Directory)

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
  deriving (Eq, Show)

compareFileDir :: String -> Integer -> String -> Integer -> Ordering
compareFileDir name1 size1 name2 size2 = case compare size1 size2 of
  EQ -> compare name1 name2
  res -> res

--showDirRecursive :: Directory -> String
--showDirRecursive = showDir' showDirRecursive

showDir :: Directory -> String
showDir = showDir' showDirBase

-- TODO sorting?
showDir' :: (Directory -> String) -> Directory -> String
showDir' show' dir@(Directory _ _ subdirs files) =
  "Current dir:\n" ++ showDirBase dir ++ 
  "\n\nFiles: \n" ++ files' ++ 
  "\n\nDirs: \n" ++ subdirs'
  where
    files' = unlines $ map showFile files
    subdirs' = unlines $ map show' subdirs
showDir' _ dir@(BadDir _ _) = showDirBase dir

-- TODO display size in GB/MB
showDirBase :: Directory -> String
showDirBase (Directory name size _ _) = showFileDir name size
showDirBase (BadDir name err) = name ++ "\t Error: " ++ show err

showFile :: File -> String
showFile (File name size) = showFileDir name size

showFileDir :: String -> Integer -> String
showFileDir name size = printf "%-30s%-10s" name (bytesToHuman size)

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
newtype Path = Path [String]

emptyPath :: Path
emptyPath = Path []

getDir :: Directory -> Path -> Directory
getDir dir (Path path) = foldl changeDir dir path

changeDir :: Directory -> String -> Directory
changeDir (Directory _ _ subDirs _) subDirName = 
  case find ((== subDirName) . getDirName) subDirs of
    Nothing -> error "Dir not found"
    Just subDir -> subDir
changeDir _ _ = error "Can't change into a bad dir"

showPath :: Path -> String
showPath (Path path) = foldr (</>) "" path