module Main where

import DataFlow.Core
import DataFlow.Graphviz.Renderer
import DataFlow.DFD

main :: IO ()
main = putStr $ renderGraphviz $ asDFD $
  Diagram (Just "Legend") [
    TrustBoundary "trust" "TrustBoundary" [
      Function "function" "Function",
      Database "database" "Database",
      InputOutput "io" "InputOutput"
    ],
    Flow "function" "io" "Flow" "",
    Flow "io" "function" "Flow" "(backwards)",
    Flow "function" "database" "Flow" ""
  ]

