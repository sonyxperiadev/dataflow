module DataFlow.Graphviz.EdgeNormalization (normalize) where

import DataFlow.Graphviz

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set

type Normalizer v = State (Set (NodeID, NodeID)) v

exists :: (NodeID, NodeID) -> Normalizer Bool
exists k = do
  s <- get
  return $ Set.member k s

register :: (NodeID, NodeID) -> Normalizer ()
register p = modify $ \s -> Set.insert p s

shouldInvert :: (NodeID, NodeID) -> Normalizer Bool
shouldInvert k@(i1, i2) = do
  e <- exists k
  if e
    then return False
    else do
      ie <- exists (i2, i1)
      if ie
        then return True
        else do
          register k
          return False

normalizeStmt :: Stmt -> Normalizer Stmt
normalizeStmt e@(EdgeStmt (EdgeExpr (IDOperand i1) op (IDOperand i2)) attrs) = do
  i <- shouldInvert (i1, i2)
  return $ if i then EdgeStmt (EdgeExpr (IDOperand i2) op (IDOperand i1))
                              (Attr "dir" "back" : attrs)
           else e

normalizeStmt (SubgraphStmt (Subgraph id' stmts)) = do
  s <- mapM normalizeStmt stmts
  return $ SubgraphStmt $ Subgraph id' s
normalizeStmt s = return s

normalize' :: Graph -> Normalizer Graph
normalize' (Digraph id' stmts) = do
  s <- mapM normalizeStmt stmts
  return $ Digraph id' s

normalize :: Graph -> Graph
normalize g = evalState (normalize' g) Set.empty
