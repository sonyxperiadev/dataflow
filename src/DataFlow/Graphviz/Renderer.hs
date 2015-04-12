{-# LANGUAGE FlexibleInstances #-}

module DataFlow.Graphviz.Renderer (
  renderGraphviz
  ) where

import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import Text.Printf
import DataFlow.Graphviz

type Indent = Int
type IndentNext = Bool
type Step = Int
data RendererState = RendererState Indent IndentNext Step

-- | The Renderer represents some output generator that runs on a 'Diagram'.
type Renderer t = WriterT [String] (State RendererState) t

class Renderable t where
  render :: t -> Renderer ()

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

instance Renderable ID where
  render (ID s) = write s

instance Renderable Attr where
  render (Attr i1 i2) = writeln $ printf "%s = %s;" (show i1) (show i2)

instance Renderable AttrList where
  render = mapM_ render

instance Renderable Port where
  render (Port (Just id') c) =
    write $ printf "%s:%s" (show id') (map toLower $ show c)
  render (Port Nothing c) =
    write $ map toLower $ show c

instance Renderable NodeID where
  render (NodeID id' (Just port)) = do
    render id'
    write ":"
    render port
  render (NodeID id' Nothing) = render id'

instance Renderable Subgraph where
  render (Subgraph id' []) =
    writeln $ printf "subgraph %s {}" (show id')
  render (Subgraph id' stmts) = do
    writeln $ printf "subgraph %s {" (show id')
    withIndent $ render stmts
    writeln "}"

instance Renderable EdgeOperator where
  render Arrow = write " -> "
  render Line = write " -- "

instance Renderable EdgeOperand where
  render (IDOperand nodeId) = render nodeId
  render (SubgraphOperand sg) = render sg

instance Renderable EdgeExpr where
  render (EdgeExpr o1 operator o2) = do
    render o1
    render operator
    render o2

instance Renderable AttrStmtType where
  render = write . map toLower . show

inBrackets :: Renderer () -> Renderer ()
inBrackets r = do
  writeln " ["
  withIndent r
  writeln "]"

instance Renderable Stmt where
  render (NodeStmt id' []) = do
    render id'
    writeln ""
  render (NodeStmt id' attrs) = do
    render id'
    inBrackets $ render attrs
  render (EdgeStmt expr []) = do
    render expr
    writeln ";"
  render (EdgeStmt expr attrs) = do
    render expr
    inBrackets $ render attrs
  render (AttrStmt t []) = do
    render t
    writeln " []"
  render (AttrStmt t attrs) = do
    render t
    inBrackets $ render attrs
  render (EqualsStmt i1 i2) = do
    render i1
    write " = "
    render i2
    writeln ";"
  render (SubgraphStmt sg) = render sg

instance Renderable StmtList where
  render = mapM_ render

instance Renderable Graph where
  render (Digraph id' stmts) = do
    writeln $ printf "digraph %s {" (show id')
    withIndent $ render stmts
    writeln "}"

renderString :: Renderer () -> String
renderString r =
  concat $ evalState (execWriterT r) (RendererState 0 False 0)

renderGraphviz :: Graph -> String
renderGraphviz = renderString . render
