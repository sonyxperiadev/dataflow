module DataFlow.DFD where

import Text.Printf
import qualified DataFlow.Core as C
import DataFlow.Graphviz
import DataFlow.Graphviz.EdgeNormalization

inQuotes :: String -> String
inQuotes s = "\"" ++ s ++ "\""

inAngleBrackets :: String -> String
inAngleBrackets s = "<" ++ s ++ ">"

label :: String -> Attr
label "" = Attr (ID "label") (ID "")
label s = Attr (ID "label") (ID $ inAngleBrackets s)

bold :: String -> String
bold "" = ""
bold s = "<b>" ++ s ++ "</b>"

italic :: String -> String
italic "" = ""
italic s = "<i>" ++ s ++ "</i>"

small :: String -> String
small "" = ""
small s = "<font point-size=\"10\">" ++ s ++ "</font>"

convertObject :: C.Object -> StmtList

convertObject (C.InputOutput id' name) = [
    NodeStmt (ID id') [
      Attr (ID "shape") (ID "square"),
      Attr (ID "style") (ID "bold"),
      label $ bold name
    ]
  ]

convertObject (C.TrustBoundary id' name objects) =
  let sgId = (ID $ "cluster_" ++ id')
      objectStmts = convertObjects objects
      sgAttrStmt = AttrStmt Graph [
          Attr (ID "fontsize") (ID "10"),
          Attr (ID "fontcolor") (ID "grey35"),
          Attr (ID "style") (ID "dashed"),
          Attr (ID "color") (ID "grey35"),
          label $ italic name
        ]
      stmts = sgAttrStmt : objectStmts
  in [SubgraphStmt $ Subgraph sgId stmts]

convertObject (C.Function id' name) =
  [
    NodeStmt (ID id') [
      Attr (ID "shape") (ID "circle"),
      label $ bold name
    ]
  ]

convertObject (C.Database id' name) =
  [
    NodeStmt (ID id') [
      Attr (ID "shape") (ID "none"),
      label $ printf "<table sides=\"TB\" cellborder=\"0\"><tr><td>%s</td></tr></table>" (bold name)
    ]
  ]

convertObject (C.Flow i1 i2 op desc) =
  [
    EdgeStmt (EdgeExpr (IDOperand (NodeID (ID i1) Nothing))
                       Arrow
                       (IDOperand (NodeID (ID i2) Nothing))) [
      label $ bold op ++ "<br/>" ++ small desc
    ]
  ]

convertObjects :: [C.Object] -> StmtList
convertObjects = concatMap convertObject

defaultGraphStmts :: StmtList
defaultGraphStmts = [
    AttrStmt Graph [
      Attr (ID "fontname") (ID "Arial"),
      Attr (ID "fontsize") (ID "14")
    ],
    AttrStmt Node [
      Attr (ID "fontname") (ID "Arial"),
      Attr (ID "fontsize") (ID "14")
    ],
    AttrStmt Edge [
      Attr (ID "shape") (ID "none"),
      Attr (ID "fontname") (ID "Arial"),
      Attr (ID "fontsize") (ID "12")
    ],
    EqualsStmt (ID "labelloc") (ID $ inQuotes "t"),
    EqualsStmt (ID "fontsize") (ID "20"),
    EqualsStmt (ID "nodesep") (ID "1"),
    EqualsStmt (ID "rankdir") (ID "t")
  ]

asDFD :: C.Diagram -> Graph
asDFD (C.Diagram (Just name) objects) =
  normalize $ Digraph (ID $ inQuotes name) $
    let lbl = EqualsStmt (ID "label") (ID $ inAngleBrackets $ bold name)
    in lbl : defaultGraphStmts ++ convertObjects objects
asDFD (C.Diagram Nothing objects) =
  normalize $ Digraph (ID "Untitled") $
    defaultGraphStmts ++ convertObjects objects
