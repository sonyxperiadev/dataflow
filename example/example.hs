module Main where

import DataFlow.Core
import DataFlow.DFD

main :: IO ()
main = printDfd $
  Diagram "My Diagram" [
    TrustBoundary "browser" "Browser" [
      Function "webapp" "Webapp"
    ],
    TrustBoundary "aws" "Amazon AWS" [
      Function "server" "Web Server",
      Database "logs" "Logs"
    ],
    InputOutput "analytics" "Google Analytics",

    Flow "webapp" "server" "Request /" "",
    Flow "server" "logs" "Log" "User IP",
    Flow "server" "webapp" "Response" "User Profile",

    Flow "webapp" "analytics" "Log" "Page Navigation"
  ]
