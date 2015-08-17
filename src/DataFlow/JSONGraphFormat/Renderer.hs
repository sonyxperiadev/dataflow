module DataFlow.JSONGraphFormat.Renderer (convertDiagram, renderJSONGraph) where

import           Data.Aeson
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.Map                 as M

import           DataFlow.Core
import qualified DataFlow.JSONGraphFormat as JG

withLabelAndMetadataFrom :: (Maybe String -> JG.Metadata -> v) -> Attributes -> v
f `withLabelAndMetadataFrom` attrs = f (M.lookup "title" attrs) (M.delete "title" attrs)

addType :: String -> JG.Metadata -> JG.Metadata
addType = M.insert "type"

addBoundary :: Maybe String -> JG.Metadata -> JG.Metadata
addBoundary (Just b) m = M.insert "trust-boundary" b m
addBoundary Nothing m = m

convertNode :: Maybe String -> Node -> JG.Node
convertNode b (InputOutput id attrs) =
  JG.Node id `withLabelAndMetadataFrom` addBoundary b (addType "io" attrs)
convertNode b (Function id attrs) =
  JG.Node id `withLabelAndMetadataFrom` addBoundary b (addType "function" attrs)
convertNode b (Database id attrs) =
  JG.Node id `withLabelAndMetadataFrom` addBoundary b (addType "database" attrs)

convertRootNode :: RootNode -> [JG.Node]
convertRootNode (Node node) = [convertNode Nothing node]
-- TODO: replace title attribute with mandatory ID for boundaries
convertRootNode (TrustBoundary id' attrs nodes) =
  map (convertNode (Just id')) nodes

convertFlow :: Flow -> JG.Edge
convertFlow (Flow source target attrs) =
  JG.Edge source target `withLabelAndMetadataFrom` attrs

convertDiagram :: Diagram -> JG.Document
convertDiagram (Diagram attrs rootNodes flows) =
  let nodes = concatMap convertRootNode rootNodes
      edges = map convertFlow flows
      graph = JG.Graph nodes edges `withLabelAndMetadataFrom` attrs
  in JG.SingleGraph graph

renderJSONGraph :: Diagram -> ByteString
renderJSONGraph = encode . convertDiagram
