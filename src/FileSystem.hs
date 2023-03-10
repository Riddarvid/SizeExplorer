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

newtype FileSystem = FS (Maybe Directory)

data Directory = Directory String Integer [Directory] [File] | BadDir String FileError

data File = File String Integer

data FileError = NoSuchFile | NoPermission | Other String
  deriving Show

--showDirRecursive :: Directory -> String
--showDirRecursive = showDir' showDirRecursive

showDir :: Directory -> String
showDir = showDir' showDirBase

showDir' :: (Directory -> String) -> Directory -> String
showDir' show' dir@(Directory _ _ subdirs files) =
  showDirBase dir ++ "\nFiles: \n" ++ files' ++ "\nDirs: \n" ++ subdirs'
  where
    files' = unlines $ map showFile files
    subdirs' = unlines $ map show' subdirs
showDir' _ dir@(BadDir _ _) = showDirBase dir

showDirBase :: Directory -> String
showDirBase (Directory name size _ _) = name ++ ", size: " ++ show size
showDirBase (BadDir name err) = name ++ "\t Error: " ++ show err

showFile :: File -> String
showFile (File name size) = name ++ ", size: " ++ show size

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