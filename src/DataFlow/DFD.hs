module DataFlow.DFD where

import Text.Printf
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

import qualified DataFlow.Core as C
import DataFlow.Attributes
import DataFlow.Graphviz
import DataFlow.Graphviz.EdgeNormalization

data DFDState = DFDState { step :: Int, clusterID :: Int }
type DFD v = State DFDState v

incrStep :: DFD ()
incrStep = modify f
  where f s = DFDState (step s + 1) (clusterID s)

incrClusterID :: DFD ()
incrClusterID = modify f
  where f s = DFDState (step s) (clusterID s + 1)

-- | Get the next \"step\" number (the order of flow arrows in the diagram).
nextStep :: DFD Int
nextStep = do
  incrStep
  liftM step get

nextClusterID :: DFD Int
nextClusterID = do
  incrClusterID
  liftM clusterID get

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

convertObject (C.InputOutput id' attrs) = return [
    NodeStmt id' [
      Attr "shape" "square",
      Attr "style" "bold",
      label $
        printf "<table border=\"0\" cellborder=\"0\" cellpadding=\"2\"><tr><td>%s</td></tr></table>"
                (bold $ getTitleOrBlank attrs)
    ]
  ]

convertObject (C.TrustBoundary attrs objects) = do
  objectStmts <- convertObjects objects
  id' <- nextClusterID
  let sgId = "cluster_" ++ show id'
      defaultSgAttrs = [
          Attr "fontsize" "10",
          Attr "fontcolor" "grey35",
          Attr "style" "dashed",
          Attr "color" "grey35"]
      sgAttrs = case getTitle attrs of
                  Just title -> defaultSgAttrs ++ [label $ italic title]
                  Nothing -> defaultSgAttrs
      sgAttrStmt = AttrStmt Graph sgAttrs
      stmts = sgAttrStmt : objectStmts
  return [SubgraphStmt $ Subgraph sgId stmts]

convertObject (C.Function id' attrs) = return [
    NodeStmt id' [
      Attr "shape" "circle",
      label $ bold $ getTitleOrBlank attrs
    ]
  ]

convertObject (C.Database id' attrs) = return [
    NodeStmt id' [
      Attr "shape" "none",
      label $ printf "<table sides=\"TB\" cellborder=\"0\"><tr><td>%s</td></tr></table>"
              (bold $ getTitleOrBlank attrs)
    ]
  ]

convertObject (C.Flow i1 i2 attrs) = do
    s <- nextStep
    let stepStr = color "#3184e4" $ bold $ printf "(%d) " s
    let text = case (M.lookup "operation" attrs, M.lookup "data" attrs) of
                (Just op, Just d) -> bold op ++ "<br/>" ++ small d
                (Just op, Nothing) -> bold op
                (Nothing, Just d) -> small d
                _ -> ""
    return [
        EdgeStmt (EdgeExpr (IDOperand (NodeID i1 Nothing))
                          Arrow
                          (IDOperand (NodeID i2 Nothing))) [
          label $ stepStr ++ text
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

convertDiagram (C.Diagram attrs objects) = do
  objs <- convertObjects objects
  return $ case M.lookup "title" attrs of
              Just title ->
                let lbl = EqualsStmt "label" (inAngleBrackets $ bold title)
                    stmts = lbl : defaultGraphStmts ++ objs
                in normalize $ Digraph (inQuotes title) stmts
              Nothing ->
                normalize $ Digraph "Untitled" $ defaultGraphStmts ++ objs

asDFD :: C.Diagram -> Graph
asDFD d = evalState (convertDiagram d) DFDState { step = 0, clusterID = 0 }

