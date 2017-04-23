module Reflex.Material.Util
  ( forId
  , idSuffix
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

-- | Returns a singleton set with id suffixed if id is present in the
-- original attrs.
idSuffix :: Text -> Map Text Text -> Map Text Text
idSuffix suffix attrs = case M.lookup "id" attrs of
  Just id -> M.singleton "id" (id <> suffix)
  Nothing -> M.empty


addClass :: [Text] -> Map Text Text -> Map Text Text
addClass cs = M.insertWith append "class" (T.unwords cs)
  where
    append "" c = c
    append p c = p <> " " <> c
