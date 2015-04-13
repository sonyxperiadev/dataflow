module Main where

import DataFlow.Core
import DataFlow.Graphviz.Renderer
import DataFlow.DFD

main :: IO ()
main = putStr $ renderGraphviz $ asDFD  $
  Diagram (Just "Webapp") [
    TrustBoundary "browser" "Browser" [
      Function "client" "Client"
    ],
    TrustBoundary "aws" "Amazon AWS" [
      Function "server" "Web Server",
      Database "logs" "Logs"
    ],
    InputOutput "analytics" "Google<br/>Analytics",

    Flow "client" "server" "Request /" "",
    Flow "server" "logs" "Log" "User IP",
    Flow "server" "client" "Response" "User Profile",

    Flow "client" "analytics" "Log" "Page Navigation"
  ]
