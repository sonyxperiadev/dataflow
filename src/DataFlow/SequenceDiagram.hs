module DataFlow.SequenceDiagram where

import qualified Data.Map as M
import Data.List.Utils
import Text.Printf

import qualified DataFlow.Core as C
import DataFlow.Attributes
import DataFlow.PlantUML

convertNewline :: String -> String
convertNewline = replace "\n" "\\n"

bold :: String -> String
bold "" = ""
bold s = printf "<b>%s</b>" s

italic :: String -> String
italic "" = ""
italic s =
  -- each line (separated by \n) needs to be wrapped in its own <i></i>
  join "\\n" $ map italic' $ split "\\n" s
  where italic' = printf "<i>%s</i>"

convertNode :: C.Node -> Stmt
convertNode (C.InputOutput id' attrs) =
  Entity id' $ convertNewline $ getTitleOrBlank attrs
convertNode (C.Function id' attrs) =
  Participant id' $ convertNewline $ getTitleOrBlank attrs
convertNode (C.Database id' attrs) =
  Database id' $ convertNewline $ getTitleOrBlank attrs

convertFlow :: C.Flow -> Stmt
convertFlow (C.Flow i1 i2 attrs) =
  let p = (convertNewline (bold $ M.findWithDefault "" "operation" attrs),
           italic $ convertNewline (M.findWithDefault "" "data" attrs))
      s = case p of
            ("", "") -> ""
            ("", d) -> d
            (o, "") -> o
            (o, d) -> o ++ "\\n" ++ d
  in Edge i1 i2 s

convertRootNode :: C.RootNode -> Stmt
convertRootNode (C.TrustBoundary attrs nodes) =
  Box (convertNewline $ M.findWithDefault "Untitled" "title" attrs) $ map convertNode nodes
convertRootNode (C.Node n) = convertNode n

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
asSequenceDiagram (C.Diagram _ rootNodes flows) =
  SequenceDiagram $ defaultSkinParams 
                    ++ map convertRootNode rootNodes 
                    ++ map convertFlow flows
