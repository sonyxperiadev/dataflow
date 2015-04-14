module Main where

import System.Environment
import DataFlow.Core
import DataFlow.Reader
import DataFlow.DFD
import DataFlow.Graphviz.Renderer

data Command = DFD | Lint

usage :: String
usage = "Usage: dataflow (dfd|lint) FILE"

dfd :: FilePath -> IO ()
dfd path = do
  res <- readDiagramFile path
  case res of
    (Left err) -> print err
    (Right diagram) -> putStr $ renderGraphviz $ asDFD diagram

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["dfd", path] -> dfd path
    _ -> fail usage
