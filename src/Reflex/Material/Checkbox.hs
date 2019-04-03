-- | Checkbox
-- https://github.com/material-components/material-components-web/tree/master/packages/mdc-checkbox

module Reflex.Material.Checkbox
  ( mdCheckbox
  , mdCheckbox'
  , mdCheckboxField
  , CheckboxConfig(..)
  , Checkbox(..)
  ) where

import Control.Lens ((^.))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M

import Reflex.Dom

import Reflex.Material.Common
import Reflex.Material.Framework (attachFormField, attachCheckbox)
import Reflex.Material.Svg
import Reflex.Material.Types
import Reflex.Material.Util

cbClass :: [Text] -> Text
cbClass ts = T.intercalate "__" ("mdc-checkbox":ts)

-- | Creates a MDC Checkbox and label within a MDC Form Field,
mdCheckboxField :: MaterialWidget t m => Bool -> CheckboxConfig t -> m () -> m (Checkbox t)
mdCheckboxField checked config children = do
  (ff, (elm, cb)) <- elAttr' "div" ("class" =: "mdc-form-field") $ do
    c <- mdCheckbox' checked config
    elDynAttr "label" (mdCheckboxLabelFor config) children
    return c
  attachFormField ff (Just elm)
  return cb

-- | Creates a MDC Checkbox on its own
mdCheckbox :: (MaterialWidget t m, SvgWidget t m) => Bool -> CheckboxConfig t -> m (Checkbox t)
mdCheckbox checked config = snd <$> mdCheckbox' checked config

mdCheckbox' :: (MaterialWidget t m, SvgWidget t m) => Bool -> CheckboxConfig t -> m (El t, Checkbox t)
mdCheckbox' checked config = do
  (elm, cb) <- elAttr' "div" ("class" =: cbClass []) $ do
    cb <- checkboxInput checked config
    divClass (cbClass ["background"]) $ do
      svgAttr "svg" ("class" =: cbClass ["checkmark"] <> "viewBox" =: "0 0 24 24") $
        svgAttr "path" ("class" =: cbClass ["checkmark-path"] <>
                        "fill" =: "none" <>
                        "stroke" =: "white" <>
                        "d" =: "M1.73,12.91 8.1,19.28 22.79,4.59") blank
      divClass (cbClass ["mixedmark"]) blank
    return cb
  attachCheckbox (Just $ isIndeterminate config) elm
  return (elm, cb)

-- This is copied from reflex-dom with the class mdc-checkbox__native-control added.
-- MDCCheckbox.init() needs the css class.
{-# INLINABLE checkboxInput #-}
checkboxInput :: (DomBuilder t m, PostBuild t m) => Bool -> CheckboxConfig t -> m (Checkbox t)
checkboxInput checked config = do
  let permanentAttrs = "type" =: "checkbox" <> "class" =: cbClass ["native-control"]
      dAttrs = M.delete "checked" . M.union permanentAttrs <$> _checkboxConfig_attributes config

  modifyAttrs <- dynamicAttributesToModifyAttributes dAttrs
  i <- inputElement $ def
    & inputElementConfig_initialChecked .~ checked
    & inputElementConfig_setChecked .~ _checkboxConfig_setValue config
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ M.mapKeys (AttributeName Nothing) permanentAttrs
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
  return $ Checkbox
    { _checkbox_value = _inputElement_checked i
    , _checkbox_change = _inputElement_checkedChange i
    }

-- Reflex-dom CheckboxConfig doesn't have "indeterminate" properly, so
-- this converts the presence of indeterminate attribute into event
-- which runs javascript to update checkbox.
isIndeterminate :: Reflex t => CheckboxConfig t -> Event t Bool
isIndeterminate config = fmap isind (updated (config ^. attributes))
  where isind = M.member "indeterminate"

mdCheckboxLabelFor :: Reflex t => CheckboxConfig t ->  Dynamic t (Map Text Text)
mdCheckboxLabelFor CheckboxConfig{..} = forId <$> _checkboxConfig_attributes
