module DataFlow.Core (
  ID,
  Attributes,
  Diagram(..),
  Node(..)
  ) where

import Data.Map as M

-- | An identifier corresponding to those in Graphviz.
type ID = String

type Attributes = M.Map String String

-- | The top level diagram.
data Diagram = Diagram Attributes [Node] deriving (Eq, Show)

-- | An node in a diagram.
data Node =
            -- | A "Input" or "Output" in DFD.
            InputOutput ID Attributes
            -- | Surrounds other nodes, denoting a boundary.
            | TrustBoundary Attributes [Node]
            -- | A \"Function\" in DFD.
            | Function ID Attributes
            -- | A \"Database\" in DFD.
            | Database ID Attributes
            -- | Describes the flow of data between two nodes.
            | Flow ID ID Attributes deriving (Show, Eq)
