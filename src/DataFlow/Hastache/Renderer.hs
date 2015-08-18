{-# LANGUAGE OverloadedStrings #-}
module DataFlow.Hastache.Renderer (renderTemplate) where

import Control.Monad
import Data.List.Utils
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.FilePath (dropExtension, takeFileName)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hastache
import Text.Hastache.Context
import Text.Markdown (markdown, def)

import DataFlow.Core

mkContextWithDefaults :: Attributes -> (String -> MuType IO) -> MuContext IO
mkContextWithDefaults attrs f =
  mkStrContext $ \key -> case f key of
                          MuNothing -> defaults key
                          v -> v
  where
    defaults "markdown" = MuLambda markdownAttr
    defaults "html_linebreaks" = MuLambda htmlLinebreaksAttr
    defaults key = case M.lookup key attrs of
                    (Just v) -> mkValue v
                    _ -> MuNothing

    mkValue :: Value -> MuType IO
    mkValue (String s) = MuVariable s
    mkValue (Array vs) =
      MuList $ map (mkStrContext . mkArrayContext) vs
      where mkArrayContext v "value" = mkValue v
            mkArrayContext v _ = MuNothing

    markdownAttr :: T.Text -> TL.Text
    markdownAttr t =
      let key = T.unpack t
          value = M.lookup key attrs
      in case value of
        (Just (String s)) -> renderHtml $ markdown def $ TL.pack s
        _ -> ""
    htmlLinebreaksAttr t =
      let key = T.unpack t
          value = M.lookup key attrs
      in case value of
        (Just (String s)) -> replace "\n" "<br/>" s
        _ -> ""

mkFlowContext :: Flow -> Int -> MuContext IO
mkFlowContext (Flow _ _ attrs) n = mkContextWithDefaults attrs ctx
  where ctx "number" = MuVariable $ show n
        ctx _ = MuNothing

mkDiagramContext :: FilePath -> Diagram -> MuContext IO
mkDiagramContext fp (Diagram attrs _ flows) =
  mkContextWithDefaults attrs ctx
  where
  ctx "filename_without_extension" = MuVariable $ dropExtension $ takeFileName fp
  ctx "flows" = MuList $ zipWith mkFlowContext flows [1..]
  ctx _ = MuNothing

renderTemplate :: String -> FilePath -> Diagram -> IO TL.Text
renderTemplate tmpl fp d =
  hastacheStr defaultConfig (encodeStr tmpl) (mkDiagramContext fp d)
