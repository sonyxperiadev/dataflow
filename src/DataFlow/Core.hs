module DataFlow.Core (
  ID,
  Name,
  Operation,
  Description,
  Diagram(..),
  Object(..)
  ) where

-- | An identifier corresponding to those in Graphviz.
type ID = String
-- | The name of a 'Diagram' or 'Object'.
type Name = String
-- | Operation heading.
type Operation = String
-- | Operation description.
type Description = String

-- | The top level diagram.
data Diagram = Diagram (Maybe Name) [Object]

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
