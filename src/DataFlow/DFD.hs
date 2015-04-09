module DataFlow.DFD where

import Control.Monad.State
import Control.Monad.Writer
import DataFlow.Core

-- | Type class for types that can be rendered as DFD.
class DFD t where
  dfd :: t -> Gen ()

instance DFD Object where
  dfd (External id' name) = objectWith brackets id' $ do
    writeln "shape = square;"
    writeln "style = bold;"
    label $ bold $ write name

  dfd (TrustBoundary id' name objects) = do
    blank
    writeln $ "subgraph cluster_" ++ id' ++ " {"
    withIndent $ do
      mapM_ dfd objects
      blank
      writeln "fontsize = 10;"
      writeln "fontcolor = grey30;"
      label $ write name
      writeln "graph[style = dashed, color=grey30];"
    writeln "}"

  dfd (Process id' name) = objectWith brackets id' $ do
    writeln "shape = circle;"
    label $ bold $ write name

  dfd (Database id' name) = objectWith brackets id' $ do
    label $
      table "sides=\"TB\" cellborder=\"0\"" $
        tr $
          td $
            bold $ write name
    writeln "shape = none;"

  dfd (Edge i1 i2 operation description) = do
    step <- nextStep
    blank
    writeln $ i1 ++ " -> " ++ i2 ++ " ["
    withIndent $
      label $ do
        bold $ write $ "(" ++ show step ++ ") " ++ operation
        write "<br/>"
        write description
    writeln "]"

instance DFD Diagram where
  dfd (Diagram title objects) = do
    writeln $ "digraph \"" ++ title ++ "\" {"
    withIndent $ do
      attrs "graph" "fontname=\"sans-serif\""
      attrs "node" "fontname=\"sans-serif\""
      attrs "edge" "fontname=\"sans-serif\", fontsize=12"
      blank

      writeln "labelloc = \"t\";"
      label $ bold $ write title
      writeln "fontsize = 20;"

      writeln "nodesep = 1;"
      writeln "rankdir = LR;"

      mapM_ dfd objects

    writeln "}"

-- | Generates the DFD output as a String.
runDfd :: Diagram -> String
runDfd diagram = concat $ evalState (execWriterT (dfd diagram)) (GenState 0 False 0)

-- | Prints the DFD output to stdout.
printDfd :: Diagram -> IO ()
printDfd = putStr . runDfd
