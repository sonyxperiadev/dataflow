module Main where

import System.Environment
import DataFlow.Reader
import DataFlow.DFD
import DataFlow.SequenceDiagram
import DataFlow.Graphviz.Renderer
import DataFlow.PlantUML.Renderer

data Command = DFD | Lint

usage :: String
usage = "Usage: dataflow (dfd|seq) FILE"

dfd :: FilePath -> IO ()
dfd path = do
  res <- readDiagramFile path
  case res of
    (Left err) -> print err
    (Right diagram) -> putStr $ renderGraphviz $ asDFD diagram

seq' :: FilePath -> IO ()
seq' path = do
  res <- readDiagramFile path
  case res of
    (Left err) -> print err
    (Right diagram) -> putStr $ renderPlantUML $ asSequenceDiagram diagram


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["dfd", path] -> dfd path
    ["seq", path] -> seq' path
    _ -> fail usage
