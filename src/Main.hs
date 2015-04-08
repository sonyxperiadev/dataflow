module Main where

import Control.Monad.State
import Control.Monad.Writer

type ID = String
type Name = String
type Operation = String
type Description = String

data Diagram = Diagram Name [Object]

data Object = Client ID Name
            | TrustBoundary ID Name [Object]
            | Process ID Name
            | Database ID Name
            | Edge ID ID Operation Description deriving (Show, Eq)

type Indent = Int
type Step = Int
data GenState = GenState Indent Step
type Gen t = WriterT [String] (State GenState) t

write :: String -> Gen ()
write s = do
  (GenState n _) <- lift get
  tell [replicate n ' ' ++ s]

incrStep :: Gen ()
incrStep = modify $ \(GenState n s') -> GenState n (s' + 1)

nextStep :: Gen Int
nextStep = do
  incrStep
  (GenState _ s) <- lift get
  return s

indent :: Gen ()
indent = modify $ \(GenState n s) -> GenState (n + 2) s

dedent :: Gen ()
dedent = modify $ \(GenState n s) -> GenState (n - 2) s

withIndent :: Gen () -> Gen ()
withIndent gen = do
  indent
  gen
  dedent

blank :: Gen ()
blank = tell [""]

label :: Gen () -> Gen ()
label contents = do
  write "label = <"
  withIndent contents
  write ">;"

tag :: String -> String -> Gen () -> Gen ()
tag t attrs contents = do
  write $ "<" ++ t ++ (if null attrs then "" else " " ++ attrs) ++ ">"
  withIndent contents
  write $ "</" ++ t ++ ">"

bold :: Gen () -> Gen ()
bold = tag "b" ""

table :: String -> Gen () -> Gen ()
table = tag "table"

tr :: Gen () -> Gen ()
tr = tag "tr" ""

td :: Gen () -> Gen ()
td = tag "td" ""

type Enclosing = (Char, Char)
brackets, curlyBrackets :: Enclosing
brackets = ('[', ']')
curlyBrackets = ('{', '}')

objectWith :: Enclosing -> ID -> Gen () -> Gen ()
objectWith (before, after) id' attributes = do
  blank
  write $ id' ++ " " ++ [before]
  withIndent attributes
  write [after]

useFont :: ID -> String -> Gen ()
useFont id' font = objectWith brackets id' $ write $ "fontname = \"" ++ font ++ "\";"

instance DFD Object where
  dfd (Client id' name) = objectWith brackets id' $ do
    write "shape = square;"
    write "style = bold;"
    label $ bold $ write name

  dfd (TrustBoundary id' name objects) = do
    blank
    write $ "subgraph cluster_" ++ id' ++ " {"
    withIndent $ do
      mapM_ dfd objects
      blank
      write $ "label = <<b>" ++ name ++ "</b>>;"
      write "graph[style = dashed];"
    write "}"

  dfd (Process id' name) = objectWith brackets id' $ do
    write "shape = circle;"
    label $ bold $ write name

  dfd (Database id' name) = objectWith brackets id' $ do
    label $
      table "sides=\"TB\" cellborder=\"0\"" $
        tr $
          td $
            bold $ write name
    write "shape = none;"

  dfd (Edge i1 i2 operation description) = do
    step <- nextStep
    blank
    write $ i1 ++ " -> " ++ i2 ++ " ["
    withIndent $
      label $ do
        bold $ write $ "(" ++ show step ++ ") " ++ operation
        write "<br/>"
        write description
    write "]"

class DFD t where
  dfd :: t -> Gen ()

instance DFD Diagram where
  dfd (Diagram title objects) = do
    write $ "digraph \"" ++ title ++ "\" {"
    withIndent $ do
      useFont "graph" "sans-serif"
      useFont "node" "sans-serif"
      useFont "edge" "sans-serif"
      blank

      write "labelloc = \"t\";"
      label $ bold $ write title

      write "rankdir = LR;"

      mapM_ dfd objects

    write "}"

runDfd :: Diagram -> String
runDfd diagram = unlines $ evalState (execWriterT (dfd diagram)) (GenState 0 0)

printDfd :: Diagram -> IO ()
printDfd = putStr . runDfd

main :: IO ()
main = printDfd $ Diagram "My Diagram" [
                    Client "phone" "My Phone",
                    TrustBoundary "aws" "AWS" [
                      Process "app_server" "App Server",
                      Database "dbfile" "Database.db"
                    ],
                    Edge "phone" "app_server" "Save" "Some stuff...",
                    Edge "app_server" "dbfile" "Upsert" "User data"
                  ]
