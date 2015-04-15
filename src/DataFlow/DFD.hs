module DataFlow.DFD where

import Text.Printf
import Control.Monad
import Control.Monad.State
import qualified DataFlow.Core as C
import DataFlow.Graphviz
import DataFlow.Graphviz.EdgeNormalization

type Step = Int
type DFD v = State Step v

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

convertObject :: C.Object -> DFD StmtList

convertObject (C.InputOutput id' name) = return [
    NodeStmt id' [
      Attr "shape" "square",
      Attr "style" "bold",
      label $ printf "<table border=\"0\" cellborder=\"0\" cellpadding=\"2\"><tr><td>%s</td></tr></table>" (bold name)
    ]
  ]

convertObject (C.TrustBoundary id' name objects) = do
  objectStmts <- convertObjects objects
  let sgId = "cluster_" ++ id'
      sgAttrStmt = AttrStmt Graph [
          Attr "fontsize" "10",
          Attr "fontcolor" "grey35",
          Attr "style" "dashed",
          Attr "color" "grey35",
          label $ italic name
        ]
      stmts = sgAttrStmt : objectStmts
  return [SubgraphStmt $ Subgraph sgId stmts]

convertObject (C.Function id' name) = return [
    NodeStmt id' [
      Attr "shape" "circle",
      label $ bold name
    ]
  ]

convertObject (C.Database id' name) = return [
    NodeStmt id' [
      Attr "shape" "none",
      label $ printf "<table sides=\"TB\" cellborder=\"0\"><tr><td>%s</td></tr></table>" (bold name)
    ]
  ]

convertObject (C.Flow i1 i2 op desc) = do
    step <- nextStep
    let stepStr = color "#3184e4" $ bold $ printf "(%d) " step
    return [
        EdgeStmt (EdgeExpr (IDOperand (NodeID i1 Nothing))
                          Arrow
                          (IDOperand (NodeID i2 Nothing))) [
          label $ stepStr ++ bold op ++ "<br/>" ++ small desc
        ]
      ]

convertObjects :: [C.Object] -> DFD StmtList
convertObjects = liftM concat . mapM convertObject

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

convertDiagram (C.Diagram (Just name) objects) = do
  let lbl = EqualsStmt "label" (inAngleBrackets $ bold name)
  objs <- convertObjects objects
  let stmts = lbl : defaultGraphStmts ++ objs
  return $ normalize $ Digraph (inQuotes name) stmts

convertDiagram (C.Diagram Nothing objects) = do
  objs <- convertObjects objects
  return $ normalize $ Digraph "Untitled" $ defaultGraphStmts ++ objs

asDFD :: C.Diagram -> Graph
asDFD d = evalState (convertDiagram d) 0

