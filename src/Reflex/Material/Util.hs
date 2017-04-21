module Reflex.Material.Util
  ( forId
  , addClass
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))

----------------------------------------------------------------------------
-- attrs utils

forId :: Map Text Text -> Map Text Text
forId attrs = case M.lookup "id" attrs of
  Just id -> M.singleton "for" id
  Nothing -> M.empty

addClass :: [Text] -> Map Text Text -> Map Text Text
-- addClass cs m = M.insert "class" (T.unwords (old:cs)) m
--   where old = M.findWithDefault "" "class" m
addClass cs = M.insertWith (<>) "class" (T.unwords ("":cs))
