module DataFlow.Attributes where

import qualified Data.Map as M

import DataFlow.Core

getTitleOrBlank :: Attributes -> String
getTitleOrBlank = show . M.findWithDefault (String "") "title"

getTitle :: Attributes -> Maybe String
getTitle a = show <$> M.lookup "title" a
