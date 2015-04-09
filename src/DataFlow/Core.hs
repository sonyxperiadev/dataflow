module DataFlow.Core where

import Control.Monad.State
import Control.Monad.Writer

type ID = String
type Name = String
type Operation = String
type Description = String

-- | The top level diagram.
data Diagram = Diagram Name [Object]

-- | An object in a diagram.
data Object = External ID Name
            | TrustBoundary ID Name [Object]
            | Process ID Name
            | Database ID Name
            | Edge ID ID Operation Description deriving (Show, Eq)

type Indent = Int
type IndentNext = Bool
type Step = Int
data GenState = GenState Indent IndentNext Step

-- | The monad stack for generating output based on Diagram.
type Gen t = WriterT [String] (State GenState) t

write :: String -> Gen ()
write s = do
  (GenState n indentNext step) <- lift get
  if indentNext
    then tell [replicate n ' ' ++ s]
    else tell [s]
  put $ GenState n False step

writeln :: String -> Gen ()
writeln s = do
  write s
  write "\n"
  modify $ \(GenState n _ s') -> GenState n True s'

incrStep :: Gen ()
incrStep = modify $ \(GenState n indentNext s') -> GenState n indentNext (s' + 1)

nextStep :: Gen Int
nextStep = do
  incrStep
  (GenState _ _ s) <- lift get
  return s

indent :: Gen ()
indent = modify $ \(GenState n indentNext s) -> GenState (n + 2) indentNext s

dedent :: Gen ()
dedent = modify $ \(GenState n indentNext s) -> GenState (n - 2) indentNext s

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
  contents
  writeln ">;"

tag :: String -> String -> Gen () -> Gen ()
tag t a contents = do
  write $ "<" ++ t ++ (if null a then "" else " " ++ a) ++ ">"
  contents
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
  writeln $ id' ++ " " ++ [before]
  withIndent attributes
  writeln [after]

attrs :: ID -> String -> Gen ()
attrs id' = objectWith brackets id' . writeln

