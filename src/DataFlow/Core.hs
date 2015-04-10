module DataFlow.Core (
  ID,
  Name,
  Operation,
  Description,
  Diagram(..),
  Object(..),
  Renderer,
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
data RendererState = RendererState Indent IndentNext Step

-- | The Renderer represents some output generator that runs on a 'Diagram'.
type Renderer t = WriterT [String] (State RendererState) t

-- | Run the 'Renderer' and get the output as a 'String'.
evalDiagram :: Renderer () -> String
evalDiagram g = concat $ evalState (execWriterT g) (RendererState 0 False 0)

-- | Write a string to the output (no linefeed).
write :: String -> Renderer ()
write s = do
  (RendererState n indentNext step) <- lift get
  if indentNext
    then tell [replicate n ' ' ++ s]
    else tell [s]
  put $ RendererState n False step

-- | Write a string to the output (with linefeed).
writeln :: String -> Renderer ()
writeln s = do
  write s
  write "\n"
  modify $ \(RendererState n _ s') -> RendererState n True s'

incrStep :: Renderer ()
incrStep = modify $ \(RendererState n indentNext s') -> RendererState n indentNext (s' + 1)

-- | Get the next \"step\" number (the order of flow arrows in the diagram).
nextStep :: Renderer Int
nextStep = do
  incrStep
  (RendererState _ _ s) <- lift get
  return s

-- | Increase indent with 2 spaces.
indent :: Renderer ()
indent = modify $ \(RendererState n indentNext s) -> RendererState (n + 2) indentNext s

-- | Decrease indent with 2 spaces.
dedent :: Renderer ()
dedent = modify $ \(RendererState n indentNext s) -> RendererState (n - 2) indentNext s

-- | Indent the output of gen with 2 spaces.
withIndent :: Renderer () -> Renderer ()
withIndent gen = do
  indent
  gen
  dedent

-- | Write a blank line.
blank :: Renderer ()
blank = tell [""]

-- | Write a label with the output of gen as its contents.
label :: Renderer () -> Renderer ()
label contents = do
  write "label = <"
  contents
  writeln ">;"

-- | Write an HTML tag t with the output of gen as its contents.
tag :: String -> String -> Renderer () -> Renderer ()
tag t a contents = do
  write $ "<" ++ t ++ (if null a then "" else " " ++ a) ++ ">"
  contents
  write $ "</" ++ t ++ ">"

-- | Write a \<b\> tag surrounding the output of another 'Renderer'.
bold :: Renderer () -> Renderer ()
bold = tag "b" ""

-- | Write a \<table\> tag, with attributes, surrounding the output of
--   another 'Renderer'.
table :: String -> Renderer () -> Renderer ()
table = tag "table"

-- | Write a \<tr\> tag surrounding the output of another 'Renderer'.
tr :: Renderer () -> Renderer ()
tr = tag "tr" ""

-- | Write a \<td\> tag surrounding the output of another 'Renderer'.
td :: Renderer () -> Renderer ()
td = tag "td" ""

-- | The enclosing characters in a block.
data Enclosing = Brackets | CurlyBrackets

-- | Write an object with the given 'Enclosing' characters, 'ID' and 'Renderer' as
--   its contents.
objectWith :: Enclosing -> ID -> Renderer () -> Renderer ()
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
attrs :: ID -> String -> Renderer ()
attrs id' = objectWith Brackets id' . writeln
