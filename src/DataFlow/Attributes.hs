module DataFlow.Attributes where

import qualified Data.Map as M
import Data.Functor ((<$>))

import DataFlow.Core

-- | Tries to find the title attribute, defaulting to a blank string.
getTitleOrBlank :: Attributes -> String
getTitleOrBlank = show . M.findWithDefault (String "") "title"

-- | Tries to find the title attribute.
getTitle :: Attributes -> Maybe String
getTitle a = show <$> M.lookup "title" a
