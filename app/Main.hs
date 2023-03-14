module Main (main) where

import Explorer ( runExplorer, Explorer, showStatus, showCaret, loadFileSystem )
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Except (MonadError(catchError))
import FileSystem (FileError (..)) -- TODO fix dependencies
import System.IO (stdout, hFlush)

main :: IO ()
main = do
  putStrLn "Welcome to SizeExplorer!"
  runExplorer explorerRepl

explorerRepl :: Explorer ()
explorerRepl = do
  printStatus
  printCaret
  getInput

printStatus :: Explorer ()
printStatus = do
  status <- showStatus
  liftIO $ putStrLn status

printCaret :: Explorer ()
printCaret = do
  caret <- showCaret
  liftIO $ putStr caret
  liftIO $ hFlush stdout

printAndRetry :: FileError -> Explorer ()
printAndRetry err = do
  liftIO $ putStrLn msg
  explorerRepl
  where
    msg = case err of
      NoSuchFile -> "File does not exist"
      NoPermission -> "You don't have permission to open that file"
      Other msg' -> "Error: " ++ msg'

-- TODO autocomplete?
getInput :: Explorer ()
getInput = do
  input <- liftIO getLine
  catchError (executeCommand input) printAndRetry

executeCommand :: String -> Explorer ()
executeCommand str = case tokens of
  [] -> do
    printCaret
    getInput
  (cmd : args) -> case cmd of
    "l" -> loadFS args
    "cd" -> cd args
    "exit" -> exit
    "q" -> exit
    "h" -> help
    _ -> instrNotFound cmd
  where
    tokens = words str

loadFS :: [String] -> Explorer ()
loadFS [dir] = do
  liftIO $ putStrLn "\nScanning directory. This might take a while.\n"
  loadFileSystem dir
  explorerRepl
loadFS _ = error "Bla" -- TODO use real error handling instead

cd :: [String] -> Explorer ()
cd = undefined

exit :: Explorer ()
exit = liftIO $ putStrLn "Bye!"

help :: Explorer ()
help = undefined

instrNotFound :: String -> Explorer ()
instrNotFound = undefined
