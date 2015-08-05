{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DataFlow.Graphviz.Renderer (
  renderGraphviz
  ) where

import Data.Char
import Data.List.Utils
import Text.Printf

import DataFlow.PrettyRenderer
import DataFlow.Graphviz

convertNewline :: String -> String
convertNewline = replace "\n" "<br/>"

class Renderable t where
  render :: t -> Renderer ()

instance Renderable Attr where
  render (Attr i1 i2) = writeln $ printf "%s = %s;" i1 (convertNewline i2)

instance Renderable AttrList where
  render = mapM_ render

instance Renderable Port where
  render (Port (Just id') c) =
    write $ printf "%s:%s" (show id') (map toLower $ show c)
  render (Port Nothing c) =
    write $ map toLower $ show c

instance Renderable NodeID where
  render (NodeID id' (Just port)) = do
    write id'
    write ":"
    render port
  render (NodeID id' Nothing) = write id'

instance Renderable Subgraph where
  render (Subgraph id' []) =
    writeln $ printf "subgraph %s {}" id'
  render (Subgraph id' stmts) = do
    writeln $ printf "subgraph %s {" id'
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
    write id'
    writeln ""
  render (NodeStmt id' attrs) = do
    write id'
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
    write i1
    write " = "
    write i2
    writeln ";"
  render (SubgraphStmt sg) = render sg

instance Renderable StmtList where
  render = mapM_ render

instance Renderable Graph where
  render (Digraph id' stmts) = do
    writeln $ printf "digraph %s {" id'
    withIndent $ render stmts
    writeln "}"

renderGraphviz :: Graph -> String
renderGraphviz = renderWithIndent . render
