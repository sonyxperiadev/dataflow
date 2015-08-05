module DataFlow.Hastache.Renderer (renderTemplate) where

import Data.List.Utils
import qualified Data.Map as M
import Text.Hastache
import Text.Hastache.Context
import System.FilePath (dropExtension)
import qualified Data.Text.Lazy as TL

import DataFlow.Core

replaceNewlines :: String -> String
replaceNewlines = replace "\n" "<br/>"

mkFlowContext :: Object -> [MuContext IO]
mkFlowContext (Flow _ _ attrs) =
  [mkStrContext ctx]
  where
  ctx key = MuVariable $ replaceNewlines $ M.findWithDefault "" key attrs
mkFlowContext _ = []

mkDiagramContext :: FilePath -> Diagram -> MuContext IO
mkDiagramContext fp (Diagram attrs objects) =
  mkStrContext ctx
  where
  ctx "filename_without_extension" = MuVariable $ dropExtension fp
  ctx "flows" = MuList $ concatMap mkFlowContext objects
  ctx key = MuVariable $ replaceNewlines $ M.findWithDefault "" key attrs

renderTemplate :: String -> FilePath -> Diagram -> IO TL.Text
renderTemplate tmpl fp d =
  hastacheStr defaultConfig (encodeStr tmpl) (mkDiagramContext fp d)
