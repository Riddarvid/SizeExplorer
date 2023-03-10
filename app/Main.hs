module Main (main) where

import Explorer ( runExplorer, Explorer, showStatus, showCaret )
import Control.Monad.IO.Class (MonadIO(liftIO))

main :: IO ()
main = do
  putStrLn "Welcome to SizeExplorer!"
  runExplorer explorerRepl

explorerRepl :: Explorer ()
explorerRepl = do
  status <- showStatus
  liftIO $ putStrLn status
  caret <- showCaret
  liftIO $ putStrLn caret
  -- TODO get user input