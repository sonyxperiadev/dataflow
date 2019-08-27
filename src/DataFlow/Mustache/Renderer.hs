{-# LANGUAGE OverloadedStrings #-}
-- | Renders diagrams using Mustache templates.
module DataFlow.Mustache.Renderer (renderTemplate) where

import Data.List.Utils
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.FilePath (dropExtension, takeFileName)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Mustache
import qualified Text.Mustache.Types as MU
import Text.Parsec.Error (ParseError)
import Text.Markdown (markdown, def)

import DataFlow.Core

mkContextWithDefaults :: Attributes -> [MU.Pair] -> MU.Value
mkContextWithDefaults attrs overrides =
  object $ defaults ++ overrides
  where
    defaults =
      [ "markdown" ~> overText markdownAttr
      , "html_linebreaks" ~> overText htmlLinebreaksAttr
      ] ++ map (\(k, v) -> T.pack k ~> mkValue v) (M.toList attrs)

    mkValue :: Value -> MU.Value
    mkValue (String s) = toMustache s
    mkValue (Array vs) =
      toMustache (map (\v -> object [ "value" ~> mkValue v ]) vs)

    markdownAttr :: T.Text -> T.Text
    markdownAttr t =
      let key = T.unpack t
          value = M.lookup key attrs
      in case value of
        (Just (String s)) -> TL.toStrict $ renderHtml $ markdown def $ TL.pack s
        _ -> ""

    htmlLinebreaksAttr :: T.Text -> T.Text
    htmlLinebreaksAttr t =
      let key = T.unpack t
          value = M.lookup key attrs
      in case value of
        (Just (String s)) -> T.pack (replace "\n" "<br/>" s)
        _ -> ""

mkFlowContext :: Flow -> Int -> MU.Value
mkFlowContext (Flow _ _ attrs) n =
  mkContextWithDefaults attrs
    [ "number" ~> show n
    ]

mkDiagramContext :: FilePath -> Diagram -> MU.Value
mkDiagramContext fp (Diagram attrs _ flows) =
  mkContextWithDefaults attrs
    [ "filename_without_extension" ~> (dropExtension $ takeFileName fp)
    , "flows" ~> zipWith mkFlowContext flows [1..]
    ]

-- | Render the given template string and 'Diagram' file path.
renderTemplate :: String -> FilePath -> Diagram -> Either ParseError T.Text
renderTemplate tmpl fp d = do
  t <- compileTemplate fp (T.pack tmpl)
  pure (substitute t (mkDiagramContext fp d))
