module Main where

import System.IO
import System.Environment
import System.Exit
import DataFlow.Reader
import DataFlow.DFD
import DataFlow.SequenceDiagram
import DataFlow.Graphviz.Renderer
import DataFlow.PlantUML.Renderer
import DataFlow.Hastache.Renderer
import qualified Data.Text.Lazy.IO as TL

usage :: IO ()
usage = hPutStrLn stderr $ unlines [
    "Usage: dataflow command args*",
    "",
    "Commands",
    "--------",
    "dfd SRC               - outputs a DFD in the Graphviz DOT format",
    "seq SRC               - outputs a sequence diagram in PlantUML format",
    "template TEMPLATE SRC - renders the TEMPLATE using data from SRC",
    "",
    "All commands print to stdout."
  ]

dfd :: FilePath -> IO ()
dfd path = do
  res <- readDiagramFile path
  case res of
    (Left err) -> print err
    (Right d) -> putStr $ renderGraphviz $ asDFD d

seq' :: FilePath -> IO ()
seq' path = do
  res <- readDiagramFile path
  case res of
    (Left err) -> print err
    (Right d) -> putStr $ renderPlantUML $ asSequenceDiagram d

template :: FilePath -> FilePath -> IO ()
template tmplPath path = do
  res <- readDiagramFile path
  tmplStr <- readFile tmplPath
  case res of
    (Left err) -> print err
    (Right d) -> renderTemplate tmplStr path d >>= TL.putStr

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["dfd", path] -> dfd path
    ["seq", path] -> seq' path
    ["template", tmplPath, path] -> template tmplPath path
    ["--help"] -> usage
    _ -> do hPutStrLn stderr "Invalid command!\n\nRun with --help to see usage."
            exitWith $ ExitFailure 1
