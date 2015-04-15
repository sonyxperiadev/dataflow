{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DataFlow.PlantUML.Renderer (
    renderPlantUML
  ) where

import Text.Printf
import DataFlow.PrettyRenderer
import DataFlow.PlantUML

class Renderable t where
  render :: t -> Renderer ()

instance Renderable Stmt where
  render (SkinParam name value) = do
    write "skinparam "
    write name
    write " "
    writeln value
  render (Box name stmts) = do
    writeln $ printf "box \"%s\"" name
    withIndent $ render stmts
    writeln "end box"
  render (Participant id' name) =
    writeln $ printf "participant \"%s\" as %s" name id'
  render (Database id' name) =
    writeln $ printf "database \"%s\" as %s" name id'
  render (Entity id' name) =
    writeln $ printf "entity \"%s\" as %s" name id'
  render (Edge i1 i2 description) =
    writeln $ printf "%s -> %s : %s" i1 i2 description


instance Renderable StmtList where
  render = mapM_ render

instance Renderable Diagram where
  render (SequenceDiagram stmts) = do
    writeln "@startuml"
    render stmts
    writeln "@enduml"

renderPlantUML :: Diagram -> String
renderPlantUML = renderWithIndent . render
