-- | "DataFlow.Graphviz" provides a model corresponding to the Graphviz language
--   described at http://www.graphviz.org/content/dot-language.
--
--   __All features in the grammar are not supported__. If you are looking for
--   a complete set of bindings to Graphviz you should have a look at
--   http://projects.haskell.org/graphviz/index.html.
module DataFlow.Graphviz where

type ID = String

data Attr = Attr ID ID deriving (Show, Eq)

type AttrList = [Attr]

data Compass = N | NE | E | SE | S | SW | W | NW | C deriving (Show, Eq, Ord)

data Port = Port (Maybe ID) Compass deriving (Show, Eq, Ord)

data NodeID = NodeID ID (Maybe Port) deriving (Show, Eq, Ord)

data Subgraph = Subgraph ID StmtList deriving (Show, Eq)

data EdgeOperator = Arrow | Line deriving (Show, Eq)

data EdgeOperand = IDOperand NodeID
                 | SubgraphOperand Subgraph deriving (Show, Eq)

data EdgeExpr = EdgeExpr EdgeOperand EdgeOperator EdgeOperand deriving (Show, Eq)

data AttrStmtType = Graph | Node | Edge deriving (Show, Eq)

data Stmt = NodeStmt ID AttrList
          | EdgeStmt EdgeExpr AttrList
          | AttrStmt AttrStmtType AttrList
          | EqualsStmt ID ID
          | SubgraphStmt Subgraph deriving (Show, Eq)

type StmtList = [Stmt]

data Graph = Digraph ID StmtList deriving (Show, Eq)
