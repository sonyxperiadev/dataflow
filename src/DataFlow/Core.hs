module DataFlow.Core (
  ID,
  Attributes,
  Diagram(..),
  Object(..)
  ) where

import Data.Map as M

-- | An identifier corresponding to those in Graphviz.
type ID = String

type Attributes = M.Map String String

-- | The top level diagram.
data Diagram = Diagram Attributes [Object] deriving (Eq, Show)

-- | An object in a diagram.
data Object =
            -- | A "Input" or "Output" in DFD.
            InputOutput ID Attributes
            -- | Surrounds other objects, denoting a boundary.
            | TrustBoundary Attributes [Object]
            -- | A \"Function\" in DFD.
            | Function ID Attributes
            -- | A \"Database\" in DFD.
            | Database ID Attributes
            -- | Describes the flow of data between two objects.
            | Flow ID ID Attributes deriving (Show, Eq)
