module DataFlow.Validation (
    ValidationError(..),
    validate
) where

import Data.Set (Set, member, insert, empty)
import Text.Printf

import DataFlow.Core

data ValidationError = UnknownID ID
                     | DuplicateDeclaration ID
                     deriving (Eq)

instance Show ValidationError where
  show (UnknownID i) = printf "Unknown ID: %s" i
  show (DuplicateDeclaration i) = printf "Duplicate declaration of ID: %s" i

getNodeIDs :: Diagram -> [ID]
getNodeIDs (Diagram _ nodes _) = concatMap getRootNodeId nodes
  where getId (InputOutput i _) = [i]
        getId (Function i _) = [i]
        getId (Database i _) = [i]
        getRootNodeId (Node node) = getId node
        getRootNodeId (TrustBoundary _ _ nodes) = concatMap getId nodes

getBoundaryIDs :: Diagram -> [ID]
getBoundaryIDs (Diagram _ nodes _) = concatMap getRootNodeId nodes
  where getRootNodeId (TrustBoundary id _ _) = [id]
        getRootNodeId _ = []

validateDuplicateIDs :: [ID] -> Either [ValidationError] (Set ID)
validateDuplicateIDs ids =
  case foldl iter (empty, []) ids of
    (seen, []) -> Right seen
    (_, errors) -> Left errors
  where iter (seen, errors) i = if i `member` seen
                                  then (seen, errors ++ [DuplicateDeclaration i])
                                  else (insert i seen, errors)

validateFlowIDs :: Diagram -> Set ID -> Either [ValidationError] ()
validateFlowIDs (Diagram _ _ flows) ids =
  case foldl iter [] flows of
    [] -> Right ()
    errors -> Left errors
  where idError i = if i `member` ids then [] else [UnknownID i]
        iter errors (Flow source target _) =
          errors ++ idError source ++ idError target

validate :: Diagram -> Either [ValidationError] Diagram
validate diagram = do
  nodeIDs <- validateDuplicateIDs (getNodeIDs diagram)
  validateDuplicateIDs (getBoundaryIDs diagram)
  validateFlowIDs diagram nodeIDs
  return diagram
