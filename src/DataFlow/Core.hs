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
data Object = Client ID Name
            | TrustBoundary ID Name [Object]
            | Process ID Name
            | Database ID Name
            | Edge ID ID Operation Description deriving (Show, Eq)

type Indent = Int
type Step = Int
data GenState = GenState Indent Step

-- | The monad stack for generating output based on Diagram.
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

