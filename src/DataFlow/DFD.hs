module DataFlow.DFD where

import Control.Monad.Identity
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import DataFlow.Core

type DFDState v = State (Map (ID, ID) Bool) v
type DFDRenderer t = DFDState (Renderer t)

-- | Type class for types that can be rendered as DFD.
class RenderDFD t where
  dfd :: t -> DFDRenderer ()

return' :: t -> DFDRenderer t
return' v = return . lift . return $ v

exists :: (ID, ID) -> DFDState Bool
exists k = do
  m <- get
  return $ case Map.lookup k m of
            (Just _) -> True
            _ -> False

register :: (ID, ID) -> DFDState ()
register k = do
  m <- get
  put $ Map.insert k True m
  return ()

shouldInvert :: (ID, ID) -> DFDState Bool
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

instance RenderDFD Object where
  dfd (InputOutput id' name) =
    return $ objectWith Brackets id' $ do
      writeln "shape = square;"
      writeln "style = bold;"
      label $ bold $ write name

  dfd (TrustBoundary id' name objects) = do
    renderObjects <- mapM dfd objects
    return $ do
      blank
      writeln $ "subgraph cluster_" ++ id' ++ " {"
      withIndent $ do
        blank
        sequence_ renderObjects
        writeln "fontsize = 10;"
        writeln "fontcolor = grey30;"
        label $ write name
        writeln "graph[style = dashed, color=grey30];"
      writeln "}"

  dfd (Function id' name) = return $ objectWith Brackets id' $ do
    writeln "shape = circle;"
    label $ bold $ write name

  dfd (Database id' name) = return $ objectWith Brackets id' $ do
    label $
      table "sides=\"TB\" cellborder=\"0\"" $
        tr $
          td $
            bold $ write name
    writeln "shape = none;"

  dfd (Flow i1 i2 operation description)= do
    back <- shouldInvert (i1, i2)
    return $ do
      step <- nextStep
      blank
      if back
        then writeln $ i2 ++ " -> " ++ i1 ++ " ["
        else writeln $ i1 ++ " -> " ++ i2 ++ " ["
      withIndent $ do
        when back $
          writeln "dir = back;"
        label $ do
          bold $ write $ "(" ++ show step ++ ") " ++ operation
          write "<br/>"
          write description
      writeln "]"

instance RenderDFD Diagram where
  dfd (Diagram title objects) =
    do
      renderObjects <- mapM dfd objects
      return $ do
        writeln $ "digraph \"" ++ title ++ "\" {"
        withIndent $ do
          attrs "graph" "fontname=\"sans-serif\""
          attrs "node" "fontname=\"sans-serif\""
          attrs "edge" "fontname=\"sans-serif\", fontsize=12"
          blank

          writeln "labelloc = \"t\";"
          label $ bold $ write title
          writeln "fontsize = 20;"

          writeln "nodesep = 1;"
          writeln "rankdir = LR;"

          sequence_ renderObjects

        writeln "}"

-- | Generates the DFD output as a String.
evalDfd :: Diagram -> String
evalDfd d = evalDiagram (evalState (dfd d) Map.empty)

-- | Prints the DFD output to stdout.
printDfd :: Diagram -> IO ()
printDfd = putStr . evalDfd
