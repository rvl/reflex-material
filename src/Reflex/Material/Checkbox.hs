{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- | Checkbox
-- https://github.com/material-components/material-components-web/tree/master/packages/mdc-checkbox

module Reflex.Material.Checkbox
  ( mdCheckbox
  , mdCheckboxField
  , CheckboxConfig(..)
  , Checkbox(..)
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M

import Reflex.Dom

import Reflex.Material.Common
import Reflex.Material.Core
import Reflex.Material.Types
import Reflex.Material.Util

cbClass :: [Text] -> Text
cbClass ts = T.intercalate "__" ("mdc-checkbox":ts)

mdCheckboxField :: MonadWidget t m => Bool -> CheckboxConfig t -> m () -> m (Checkbox t)
mdCheckboxField checked config children = do
  (el, cb) <- elAttr' "div" ("class" =: "mdc-form-field") $ do
    cb <- mdCheckbox checked config
    elDynAttr "label" (mdCheckboxLabelFor config) $ children
    return cb
  attachFormField el
  return cb

mdCheckbox :: MonadWidget t m => Bool -> CheckboxConfig t -> m (Checkbox t)
mdCheckbox checked config = do
  (el, cb) <- elAttr' "div" ("class" =: cbClass []) $ do
    let config' = mdCheckboxConfig config
    cb <- checkbox checked config'
    divClass (cbClass ["background"]) $ do
      element "svg" (def & namespace .~ ns & initialAttributes .~ ((AttributeName Nothing "class") =: cbClass ["checkmark"] <> (AttributeName ns "viewBox") =: "0 0 24 24")) $
        element "path" (def & namespace .~ ns & initialAttributes .~ ((AttributeName Nothing "class") =: cbClass ["checkmark", "path"] <> (AttributeName ns "fill") =: "none" <> (AttributeName ns "stroke") =: "white" <> (AttributeName ns "d") =: "M1.73,12.91 8.1,19.28 22.79,4.59")) blank
      divClass (cbClass ["mixedmark"]) blank
    return cb
  attachCheckbox el
  return cb

ns :: Maybe Namespace
ns = Just "http://www.w3.org/2000/svg"

mdCheckboxConfig :: Reflex t => CheckboxConfig t -> CheckboxConfig t
mdCheckboxConfig c@CheckboxConfig{..} = c { _checkboxConfig_attributes = attrs' }
  where
    attrs' = addClass [cbClass ["native-control"]] <$> _checkboxConfig_attributes

mdCheckboxLabelFor :: Reflex t => CheckboxConfig t ->  Dynamic t (Map Text Text)
mdCheckboxLabelFor c@CheckboxConfig{..} = forId <$> _checkboxConfig_attributes
