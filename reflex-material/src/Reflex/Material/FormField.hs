-- | Form Field
-- https://github.com/material-components/material-components-web/blob/master/packages/mdc-form-field

module Reflex.Material.FormField
  ( mdFormField
  , mdFormFieldAlignEnd
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M

import Reflex.Dom

import Reflex.Material.Common
import Reflex.Material.Framework (attachFormField)

mdFormField :: MaterialWidget t m
            => Text -- ^ input id for label
            -> Text -- ^ label text
            -> m a  -- ^ contents
            -> m a
mdFormField = mdFormFieldBase False

mdFormFieldAlignEnd :: MaterialWidget t m => Text -> Text -> m a -> m a
mdFormFieldAlignEnd = mdFormFieldBase True

mdFormFieldBase :: MaterialWidget t m => Bool -> Text -> Text -> m a -> m a
mdFormFieldBase end forId label children = do
  (el, a) <- elAttr' "div" (mdFormFieldAttrs end) $ do
    a <- children
    elAttr "label" ("for" =: forId) $ text label
    return a
  attachFormField el Nothing
  return a

mdFormFieldAttrs :: Bool -> Map Text Text
mdFormFieldAttrs end = "class" =: (cls <> if end then cls <> "--align-end" else "")
  where cls = "mdc-form-field"
