module DataFlow.SequenceDiagram where

import Text.Printf
import qualified DataFlow.Core as C
import DataFlow.PlantUML

convertObject :: C.Object -> Stmt
convertObject (C.InputOutput id' name) =
  Entity id' name
convertObject (C.TrustBoundary _ name objects) =
  Box name $ map convertObject objects
convertObject (C.Function id' name) =
  Participant id' name
convertObject (C.Database id' name) =
  Database id' name
convertObject (C.Flow i1 i2 op desc) =
  Edge i1 i2 $ printf "<b>%s</b>\\n%s" op desc

defaultSkinParams :: [Stmt]
defaultSkinParams = [
    SkinParam "Shadowing" "false",
    SkinParam "SequenceMessageAlign" "center",
    SkinParam "DefaultFontStyle" "bold",
    SkinParam "DefaultFontColor" "#333333",

    SkinParam "NoteBackgroundColor" "#fbfb77",
    SkinParam "NoteBorderColor" "#cbcb47",

    SkinParam "NoteBackgroundColor" "#ffffcd",
    SkinParam "NoteBorderColor" "#a9a980",
    SkinParam "NoteFontColor" "#676735",
    SkinParam "NoteFontStyle" "italic",

    SkinParam "SequenceArrowColor" "#555555",
    SkinParam "SequenceArrowFontColor" "#555555",
    SkinParam "SequenceArrowFontStyle" "none",

    SkinParam "SequenceBoxBackgroundColor" "#fafafa",
    SkinParam "SequenceBoxBorderColor" "#eeeeee",
    SkinParam "SequenceBoxFontColor" "#666666",
    SkinParam "SequenceBoxFontSize" "12",
    SkinParam "SequenceBoxFontStyle" "italic",

    SkinParam "ParticipantBackgroundColor" "#dde5ff",
    SkinParam "ParticipantBorderColor" "#cccccc",
    SkinParam "ParticipantFontColor" "#333333",
    SkinParam "ParticipantFontStyle" "bold",

    SkinParam "DatabaseBackgroundColor" "#df4646",
    SkinParam "DatabaseFontColor" "#red",
    SkinParam "DatabaseFontStyle" "bold",

    SkinParam "EntityBackgroundColor" "#999999",

    SkinParam "SequenceLifeLineBorderColor" "#444444"
  ]

asSequenceDiagram :: C.Diagram -> Diagram
asSequenceDiagram (C.Diagram _ objects) =
  SequenceDiagram $ defaultSkinParams ++ map convertObject objects
