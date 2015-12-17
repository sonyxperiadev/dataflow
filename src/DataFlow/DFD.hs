-- | Convert a DataFlow 'C.Diagram' to a Graphviz 'Graph'.
module DataFlow.DFD (
  asDFD
) where

import Text.Printf
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

import qualified DataFlow.Core as C
import DataFlow.Attributes
import DataFlow.Graphviz
import DataFlow.Graphviz.EdgeNormalization

type DFDState = Int
type DFD v = State DFDState v

incrStep :: DFD ()
incrStep = modify (+ 1)

-- | Get the next \"step\" number (the sequence number of flow arrows in the
-- | diagram).
nextStep :: DFD Int
nextStep = do
  incrStep
  get

inQuotes :: String -> String
inQuotes s = "\"" ++ s ++ "\""

inAngleBrackets :: String -> String
inAngleBrackets s = "<" ++ s ++ ">"

label :: String -> Attr
label "" = Attr "label" ""
label s = Attr "label" $ inAngleBrackets s

bold :: String -> String
bold "" = ""
bold s = "<b>" ++ s ++ "</b>"

italic :: String -> String
italic "" = ""
italic s = "<i>" ++ s ++ "</i>"

small :: String -> String
small "" = ""
small s = printf "<font point-size=\"10\">%s</font>" s

-- | Display the text with the given color (Graphviz color format, e.g. @grey35@).
color :: String -> String -> String
color _ "" = ""
color c s = printf "<font color=\"%s\">%s</font>" c s

convertNode :: C.Node -> DFD StmtList

convertNode (C.InputOutput id' attrs) = return [
    NodeStmt id' [
      Attr "shape" "square",
      Attr "style" "bold",
      label $
        printf "<table border=\"0\" cellborder=\"0\" cellpadding=\"2\"><tr><td>%s</td></tr></table>"
                (bold $ getTitleOrBlank attrs)
    ]
  ]

convertNode (C.Function id' attrs) = return [
    NodeStmt id' [
      Attr "shape" "circle",
      label $ bold $ getTitleOrBlank attrs
    ]
  ]

convertNode (C.Database id' attrs) = return [
    NodeStmt id' [
      Attr "shape" "none",
      label $ printf "<table sides=\"TB\" cellborder=\"0\"><tr><td>%s</td></tr></table>"
              (bold $ getTitleOrBlank attrs)
    ]
  ]

convertNodes :: [C.Node] -> DFD StmtList
convertNodes = liftM concat . mapM convertNode

convertFlow :: C.Flow -> DFD StmtList
convertFlow (C.Flow i1 i2 attrs) = do
    s <- nextStep
    let stepStr = color "#3184e4" $ bold $ printf "(%d) " s

        asRows :: C.Value -> [String]
        asRows (C.String s) = lines s
        asRows (C.Array vs) = concatMap asRows vs

        rowsToTable :: [String] -> String
        rowsToTable rows =
          printf "<table border=\"0\" cellborder=\"0\" cellpadding=\"2\">%s</table>" r
          where r = concatMap (printf "<tr><td>%s</td></tr>") rows :: String

        rows = case (M.lookup "operation" attrs, M.lookup "data" attrs) of
                (Just op, Just d) -> (stepStr ++ bold (show op)) : map small (asRows d)
                (Just op, Nothing) -> [stepStr ++ bold (show op)]
                (Nothing, Just d) -> stepStr : map small (asRows d)
                _ -> []
    return [
        EdgeStmt (EdgeExpr (IDOperand (NodeID i1 Nothing))
                          Arrow
                          (IDOperand (NodeID i2 Nothing))) [
          label $ rowsToTable rows
        ]
      ]

convertFlows :: [C.Flow] -> DFD StmtList
convertFlows = liftM concat . mapM convertFlow

convertRootNode :: C.RootNode -> DFD StmtList
convertRootNode (C.TrustBoundary id' attrs nodes) = do
  nodeStmts <- convertNodes nodes
  let sgId = "cluster_" ++ id'
      defaultSgAttrs = [
          Attr "fontsize" "10",
          Attr "fontcolor" "grey35",
          Attr "style" "dashed",
          Attr "color" "grey35"]
      sgAttrs = case getTitle attrs of
                  Just title -> defaultSgAttrs ++ [label $ italic title]
                  Nothing -> defaultSgAttrs
      sgAttrStmt = AttrStmt Graph sgAttrs
      stmts = sgAttrStmt : nodeStmts
  return [SubgraphStmt $ Subgraph sgId stmts]

convertRootNode (C.Node n) = convertNode n

convertRootNodes :: [C.RootNode] -> DFD StmtList
convertRootNodes = liftM concat . mapM convertRootNode

defaultGraphStmts :: StmtList
defaultGraphStmts = [
    AttrStmt Graph [
      Attr "fontname" "Arial",
      Attr "fontsize" "14"
    ],
    AttrStmt Node [
      Attr "fontname" "Arial",
      Attr "fontsize" "14"
    ],
    AttrStmt Edge [
      Attr "shape" "none",
      Attr "fontname" "Arial",
      Attr "fontsize" "12"
    ],
    EqualsStmt "labelloc" (inQuotes "t"),
    EqualsStmt "fontsize" "20",
    EqualsStmt "nodesep" "1",
    EqualsStmt "rankdir" "t"
  ]

convertDiagram :: C.Diagram -> DFD Graph
convertDiagram (C.Diagram attrs rootNodes flows) = do
  n <- convertRootNodes rootNodes
  f <- convertFlows flows
  return $ case M.lookup "title" attrs of
              Just title ->
                let lbl = EqualsStmt "label" (inAngleBrackets $ show title)
                    stmts = lbl : defaultGraphStmts ++ n ++ f
                in normalize $ Digraph (inQuotes $ show title) stmts
              Nothing ->
                normalize $ Digraph "Untitled" $ defaultGraphStmts ++ n ++ f

-- | Converts a 'C.Diagram' to a 'Graph', with predefined styling, that can be
--   rendered as a Graphviz document.
asDFD :: C.Diagram -> Graph
asDFD d = evalState (convertDiagram d) 0

