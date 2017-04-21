{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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

{-
mdcCheckbox_ :: CssClass
mdcCheckbox_ = CssClass "mdc-checkbox"

checkbox_ :: MonadWidget t m => CssClass -> Text -> Bool -> CheckboxConfig t -> m (Checkbox t)
checkbox_ t domid checked config =
  elAttr "div" ("class" =: unCssClass (mdcCheckbox_ <> t)) $ do
    elAttr "input" ("type" =: "checkbox" <>
                     "id" =: domid <>
                     "class" =: cbClass ["native-control"]) blank
    cb <- checkbox checked config
    divClass (cbClassAttr ["background"]) $ do
      elAttr "svg" (cbClassAttr ["checkmark"] <> "viewBox" =: "0 0 24 24") $
        elAttr "path" (cbClassAttr ["checkmark", "path"] <>
                        "fill" =: "none" <>
                        "stroke" =: "white" <>
                        "d" =: "M1.73,12.91 8.1,19.28 22.79,4.59") blank
      elAttr "div" (cbClassAttr ["mixedmark"]) blank
    return cb

checkboxField_ :: MonadWidget t m => CssClass -> Text -> m ()
checkboxField_ t v = checkboxField'_ t v >> pure ()

checkboxField'_ :: MonadWidget t m => CssClass -> Text -> m (El t, ())
checkboxField'_ t v = elAttr' "div" ("class" =: "mdc-form-field") $ do
                         checkbox_ t "my-checkbox" False def
                         elAttr "label" ("for" =: "my-checkbox") $ text v

cbClassAttr :: [Text] -> Map Text Text
cbClassAttr = classAttr . CssClass . cbClass
-}

cbClass :: [Text] -> Text
cbClass ts = T.intercalate "__" ("mdc-checkbox":ts)

mdCheckboxField :: MonadWidget t m => Bool -> CheckboxConfig t -> m () -> m (Checkbox t)
mdCheckboxField checked config children = divClass "mdc-form-field" $ do
  cb <- mdCheckbox checked config
  elDynAttr "label" (mdCheckboxLabelFor config) $ children
  return cb

mdCheckboxLabelFor :: Reflex t => CheckboxConfig t ->  Dynamic t (Map Text Text)
mdCheckboxLabelFor c@CheckboxConfig{..} = forId <$> _checkboxConfig_attributes

mdCheckboxConfig :: Reflex t => CheckboxConfig t -> CheckboxConfig t
mdCheckboxConfig c@CheckboxConfig{..} = c { _checkboxConfig_attributes = attrs' }
  where
    attrs' = addClass [cbClass ["native-control"]] <$> _checkboxConfig_attributes

mdCheckbox :: MonadWidget t m => Bool -> CheckboxConfig t -> m (Checkbox t)
mdCheckbox checked config = do
  (el, cb) <- elAttr' "div" ("class" =: cbClass []) $ do
    let config' = mdCheckboxConfig config
    cb <- checkbox checked config'
    divClass (cbClass ["background"]) $ do
      elAttr "svg" ("class" =: cbClass ["checkmark"] <> "viewBox" =: "0 0 24 24") $
        elAttr "path" ("class" =: cbClass ["checkmark", "path"] <>
                       "fill" =: "none" <>
                       "stroke" =: "white" <>
                       "d" =: "M1.73,12.91 8.1,19.28 22.79,4.59") blank
      divClass (cbClass ["mixedmark"]) blank
    return cb
  attachCheckbox el
  return cb

{-
<div class="mdc-checkbox">
  <input type="checkbox"
         class="mdc-checkbox__native-control"/>
  <div class="mdc-checkbox__background">
    <svg class="mdc-checkbox__checkmark"
         viewBox="0 0 24 24">
      <path class="mdc-checkbox__checkmark__path"
            fill="none"
            stroke="white"
            d="M1.73,12.91 8.1,19.28 22.79,4.59"/>
    </svg>
    <div class="mdc-checkbox__mixedmark"></div>
  </div>
</div>
-}
