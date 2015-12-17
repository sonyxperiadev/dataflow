-- Renders 'Diagram' as JSON.
module DataFlow.JSONGraphFormat.Renderer (convertDiagram, renderJSONGraph) where

import qualified Data.Aeson               as A
import           Data.ByteString.Lazy     (ByteString)
import           Data.Text (pack, unpack)
import qualified Data.Map                 as M
import qualified Data.Vector              as V

import           DataFlow.Core
import qualified DataFlow.JSONGraphFormat as JG

getTitle :: JG.Metadata -> Maybe String
getTitle m =  do
  v <- M.lookup "title" m
  case v of
    (JG.Str s) -> Just s
    _ -> Nothing

convertValue :: Value -> JG.Val
convertValue (String s) = JG.Str s
convertValue (Array vs) = JG.Arr (map convertValue vs)

convertAttrs :: Attributes -> M.Map String JG.Val
convertAttrs = M.map convertValue

withLabelAndMetadataFrom :: (Maybe String -> JG.Metadata -> v) -> JG.Metadata -> v
f `withLabelAndMetadataFrom` metadata = f (getTitle metadata) (M.delete "title" metadata)

addType :: String -> JG.Metadata -> JG.Metadata
addType s = M.insert "type" (JG.Str s)

addBoundary :: Maybe String -> JG.Metadata -> JG.Metadata
addBoundary (Just b) m = M.insert "trust-boundary" (JG.Str b) m
addBoundary Nothing m = m

convertNode :: Maybe String -> Node -> JG.Node
convertNode b (InputOutput id attrs) =
  JG.Node id `withLabelAndMetadataFrom` addBoundary b (addType "io" $ convertAttrs attrs)
convertNode b (Function id attrs) =
  JG.Node id `withLabelAndMetadataFrom` addBoundary b (addType "function" $ convertAttrs attrs)
convertNode b (Database id attrs) =
  JG.Node id `withLabelAndMetadataFrom` addBoundary b (addType "database" $ convertAttrs attrs)

convertRootNode :: RootNode -> [JG.Node]
convertRootNode (Node node) = [convertNode Nothing node]
-- TODO: replace title attribute with mandatory ID for boundaries
convertRootNode (TrustBoundary id' attrs nodes) =
  map (convertNode (Just id')) nodes

convertFlow :: Flow -> JG.Edge
convertFlow (Flow source target attrs) =
  JG.Edge source target `withLabelAndMetadataFrom` convertAttrs attrs

convertDiagram :: Diagram -> JG.Document
convertDiagram (Diagram attrs rootNodes flows) =
  let nodes = concatMap convertRootNode rootNodes
      edges = map convertFlow flows
      graph = JG.Graph nodes edges `withLabelAndMetadataFrom` convertAttrs attrs
  in JG.SingleGraph graph

-- Render the 'Diagram' as a JSON 'ByteString'.
renderJSONGraph :: Diagram -> ByteString
renderJSONGraph = A.encode . convertDiagram
