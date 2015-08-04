module DataFlow.Attributes where

import qualified Data.Map as M

import DataFlow.Core

getTitleOrBlank :: Attributes -> String
getTitleOrBlank = M.findWithDefault "" "title"

getTitle :: Attributes -> Maybe String
getTitle = M.lookup "title"
