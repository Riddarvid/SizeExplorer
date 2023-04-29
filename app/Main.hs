module Main (main) where

import Explorer ( runExplorer, Explorer, showStatus, showCaret, loadFileSystem, ExplorerError (CmdUsgErr), moveToRoot, moveTo )
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Except (MonadError(catchError, throwError))
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

printAndRetry :: ExplorerError -> Explorer ()
printAndRetry err = do
  liftIO $ print err
  explorerRepl

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
  (cmd : args) -> executeCommand' cmd args
  where
    tokens = words str

executeCommand' :: String -> [String] -> Explorer ()
executeCommand' cmd args = case cmd of
  "l" -> loadFS args
  "cd" -> cd args
  "exit" -> exit
  "q" -> exit
  "h" -> help
  _ -> instrNotFound cmd

loadFS :: [String] -> Explorer ()
loadFS [dir] = do
  liftIO $ putStrLn "\nScanning directory. This might take a while.\n"
  loadFileSystem dir
  explorerRepl
loadFS _ = error "Bla" -- TODO use real error handling instead

cd :: [String] -> Explorer ()
cd args = do
  action
  explorerRepl
  where
    action = case args of
      [] -> moveToRoot
      [dir] -> moveTo dir
      _ -> throwError (CmdUsgErr "Usage: cd dir")


exit :: Explorer ()
exit = liftIO $ putStrLn "Bye!"

help :: Explorer ()
help = undefined

instrNotFound :: String -> Explorer ()
instrNotFound = undefined
