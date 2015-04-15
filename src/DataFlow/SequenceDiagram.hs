module DataFlow.SequenceDiagram where

import Text.Printf
import qualified DataFlow.Core as C
import DataFlow.PlantUML
import Data.List.Utils

convertNewline :: String -> String
convertNewline = replace "<br/>" "\\n"

bold :: String -> String
bold "" = ""
bold s = printf "<b>%s</b>" s

italic :: String -> String
italic "" = ""
italic s =
  -- each line (separated by \n) needs to be wrapped in its own <i></i>
  join "\\n" $ map italic' $ split "\\n" s
  where italic' = printf "<i>%s</i>"

convertObject :: C.Object -> Stmt
convertObject (C.InputOutput id' name) =
  Entity id' $ convertNewline name
convertObject (C.TrustBoundary _ name objects) =
  Box (convertNewline name) $ map convertObject objects
convertObject (C.Function id' name) =
  Participant id' $ convertNewline name
convertObject (C.Database id' name) =
  Database id' $ convertNewline name
convertObject (C.Flow i1 i2 op desc) =
  let p = (convertNewline (bold op), italic $ convertNewline desc)
      s = case p of
            ("", "") -> ""
            ("", d) -> d
            (o, "") -> o
            (o, d) -> o ++ "\\n" ++ d
  in Edge i1 i2 s

defaultSkinParams :: [Stmt]
defaultSkinParams = [
    SkinParam "BackgroundColor" "#white",
    SkinParam "Shadowing" "false",
    SkinParam "SequenceMessageAlign" "center",
    SkinParam "DefaultFontName" "Arial",
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

    SkinParam "SequenceLifeLineBorderColor" "#bbbbbb"
  ]

asSequenceDiagram :: C.Diagram -> Diagram
asSequenceDiagram (C.Diagram _ objects) =
  SequenceDiagram $ defaultSkinParams ++ map convertObject objects
