module Main where

import DataFlow.Core
import DataFlow.DFD

main :: IO ()
main = printDfd $
  Diagram "Legend" [
    TrustBoundary "trust" "TrustBoundary" [
      Function "function" "Function",
      Database "database" "Database",
      InputOutput "io" "InputOutput"
    ],
    Flow "function" "io" "Flow" "",
    Flow "function" "database" "Flow" ""
  ]

