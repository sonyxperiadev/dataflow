module DataFlow.PlantUML where

type ID = String
type Name = String

data Stmt = SkinParam String String
          | Box Name StmtList
          | Participant ID Name
          | Database ID Name
          | Entity ID Name
          | Edge ID ID String deriving (Show, Eq)

type StmtList = [Stmt]

data Diagram = SequenceDiagram StmtList deriving (Show, Eq)
