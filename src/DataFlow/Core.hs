module DataFlow.Core (
  ID,
  Name,
  Operation,
  Description,
  Diagram(..),
  Object(..),
  Gen,
  evalDiagram,
  write,
  writeln,
  nextStep,
  indent,
  dedent,
  withIndent,
  blank,
  label,
  tag,
  bold,
  table,
  tr,
  td,
  Enclosing(..),
  objectWith,
  attrs
  ) where

import Control.Monad.State
import Control.Monad.Writer

-- | An identifier corresponding to those in Graphviz.
type ID = String
-- | The name of a 'Diagram' or 'Object'.
type Name = String
-- | Operation heading.
type Operation = String
-- | Operation description.
type Description = String

-- | The top level diagram.
data Diagram = Diagram Name [Object]

-- | An object in a diagram.
data Object =
            -- | A "Input" or "Output" in DFD.
            InputOutput ID Name
            -- | Surrounds other objects, denoting a boundary.
            | TrustBoundary ID Name [Object]
            -- | A \"Function\" in DFD.
            | Function ID Name
            -- | A \"Database\" in DFD.
            | Database ID Name
            -- | Describes the flow of data between two objects.
            | Flow ID ID Operation Description deriving (Show, Eq)

type Indent = Int
type IndentNext = Bool
type Step = Int
data GenState = GenState Indent IndentNext Step

-- | The Gen represents some output generator that runs on a 'Diagram'..
type Gen t = WriterT [String] (State GenState) t

-- | Run the 'Gen' and get the output as a 'String'.
evalDiagram :: Gen () -> String
evalDiagram g = concat $ evalState (execWriterT g) (GenState 0 False 0)

-- | Write a string to the output (no linefeed).
write :: String -> Gen ()
write s = do
  (GenState n indentNext step) <- lift get
  if indentNext
    then tell [replicate n ' ' ++ s]
    else tell [s]
  put $ GenState n False step

-- | Write a string to the output (with linefeed).
writeln :: String -> Gen ()
writeln s = do
  write s
  write "\n"
  modify $ \(GenState n _ s') -> GenState n True s'

incrStep :: Gen ()
incrStep = modify $ \(GenState n indentNext s') -> GenState n indentNext (s' + 1)

-- | Get the next \"step\" number (the order of flow arrows in the diagram).
nextStep :: Gen Int
nextStep = do
  incrStep
  (GenState _ _ s) <- lift get
  return s

-- | Increase indent with 2 spaces.
indent :: Gen ()
indent = modify $ \(GenState n indentNext s) -> GenState (n + 2) indentNext s

-- | Decrease indent with 2 spaces.
dedent :: Gen ()
dedent = modify $ \(GenState n indentNext s) -> GenState (n - 2) indentNext s

-- | Indent the output of gen with 2 spaces.
withIndent :: Gen () -> Gen ()
withIndent gen = do
  indent
  gen
  dedent

-- | Write a blank line.
blank :: Gen ()
blank = tell [""]

-- | Write a label with the output of gen as its contents.
label :: Gen () -> Gen ()
label contents = do
  write "label = <"
  contents
  writeln ">;"

-- | Write an HTML tag t with the output of gen as its contents.
tag :: String -> String -> Gen () -> Gen ()
tag t a contents = do
  write $ "<" ++ t ++ (if null a then "" else " " ++ a) ++ ">"
  contents
  write $ "</" ++ t ++ ">"

-- | Write a \<b\> tag surrounding the output of another 'Gen'.
bold :: Gen () -> Gen ()
bold = tag "b" ""

-- | Write a \<table\> tag, with attributes, surrounding the output of
--   another 'Gen'.
table :: String -> Gen () -> Gen ()
table = tag "table"

-- | Write a \<tr\> tag surrounding the output of another 'Gen'.
tr :: Gen () -> Gen ()
tr = tag "tr" ""

-- | Write a \<td\> tag surrounding the output of another 'Gen'.
td :: Gen () -> Gen ()
td = tag "td" ""

-- | The enclosing characters in a block.
data Enclosing = Brackets | CurlyBrackets

-- | Write an object with the given 'Enclosing' characters, 'ID' and 'Gen' as
--   its contents.
objectWith :: Enclosing -> ID -> Gen () -> Gen ()
objectWith enc id' attributes =
  do
    blank
    writeln $ id' ++ " " ++ before enc
    withIndent attributes
    writeln $ after enc
  where before Brackets = "["
        before CurlyBrackets = "{"
        after Brackets = "]"
        after CurlyBrackets = "}"

-- | Write an attributes declaration for the given 'ID'.
attrs :: ID -> String -> Gen ()
attrs id' = objectWith Brackets id' . writeln
