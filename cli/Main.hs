module Main where

import System.IO
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text.Lazy.IO as TL

import DataFlow.Reader
import DataFlow.Core
import qualified DataFlow.Validation as V
import qualified DataFlow.DFD as DFD
import qualified DataFlow.SequenceDiagram as SEQ
import qualified DataFlow.Graphviz.Renderer as GVR
import qualified DataFlow.PlantUML.Renderer as PUR
import qualified DataFlow.Hastache.Renderer as HR
import qualified DataFlow.JSONGraphFormat.Renderer as JG

usage :: IO ()
usage = hPutStrLn stderr $ unlines [
    "Usage: dataflow command args*",
    "",
    "Commands",
    "--------",
    "dfd SRC               - outputs a DFD in the Graphviz DOT format",
    "seq SRC               - outputs a sequence diagram in PlantUML format",
    "template TEMPLATE SRC - renders the TEMPLATE using data from SRC",
    "json SRC              - outputs a sequence diagram in JSON Graph Format (http://jsongraphformat.info/)",
    "validate SRC          - validates the input",
    "",
    "All commands print to stdout."
  ]

showErrors :: Show s => Either [s] v -> Either String v
showErrors = either (Left . unlines . map show) Right

readAndValidate :: FilePath -> IO (Either String Diagram)
readAndValidate path = do
  res <- readDiagramFile path
  case res of
    (Left err) -> return $ Left $ show err
    (Right d) -> return (showErrors $ V.validate d)

dfd :: FilePath -> IO ()
dfd path = do
  res <- readAndValidate path
  case res of
    (Left err) -> putStrLn err
    (Right d) -> putStr $ GVR.renderGraphviz $ DFD.asDFD d

seq' :: FilePath -> IO ()
seq' path = do
  res <- readAndValidate path
  case res of
    (Left err) -> putStrLn err
    (Right d) -> putStr $ PUR.renderPlantUML $ SEQ.asSequenceDiagram d

template :: FilePath -> FilePath -> IO ()
template tmplPath path = do
  res <- readAndValidate path
  tmplStr <- readFile tmplPath
  case res of
    (Left err) -> putStrLn err
    (Right d) -> HR.renderTemplate tmplStr path d >>= TL.putStr

json :: FilePath -> IO ()
json path = do
  res <- readAndValidate path
  case res of
    (Left err) -> putStrLn err
    (Right d) -> BC.putStrLn $ JG.renderJSONGraph d

validate :: FilePath -> IO ()
validate path = do
  res <- readAndValidate path
  case res of
    (Left err) -> putStrLn err
    (Right _) -> return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["dfd", path] -> dfd path
    ["seq", path] -> seq' path
    ["template", tmplPath, path] -> template tmplPath path
    ["json", path] -> json path
    ["validate", path] -> validate path
    ["--help"] -> usage
    _ -> do hPutStrLn stderr "Invalid command!\n\nRun with --help to see usage."
            exitWith $ ExitFailure 1
