module DataFlow.DFD where

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

-- | Get the next \"step\" number (the order of flow arrows in the diagram).
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

color :: String -> String -> String
color _ "" = ""
color c s = printf "<font color=\"%s\">%s</font>" c s

showValue :: C.Value -> String
showValue (C.String s) = s
showValue (C.Array vs) = unlines $ map (("* " ++) . showValue) vs

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
    let text = case (M.lookup "operation" attrs, M.lookup "data" attrs) of
                (Just op, Just d) -> bold (showValue op) ++
                                     "<br/>" ++
                                     small (showValue d)
                (Just op, Nothing) -> bold $ showValue op
                (Nothing, Just d) -> small $ showValue d
                _ -> ""
    return [
        EdgeStmt (EdgeExpr (IDOperand (NodeID i1 Nothing))
                          Arrow
                          (IDOperand (NodeID i2 Nothing))) [
          label $ stepStr ++ text
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
                let lbl = EqualsStmt "label" (inAngleBrackets $ showValue title)
                    stmts = lbl : defaultGraphStmts ++ n ++ f
                in normalize $ Digraph (inQuotes $ showValue title) stmts
              Nothing ->
                normalize $ Digraph "Untitled" $ defaultGraphStmts ++ n ++ f

asDFD :: C.Diagram -> Graph
asDFD d = evalState (convertDiagram d) 0

