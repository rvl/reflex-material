{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Material.Radio
  ( mdRadio
  , MdRadioConfig(..)
  , MdRadio(..)
  ) where

import Data.Map
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Default (Default(..))

import Reflex.Dom
import Reflex.Dom.Builder.Class (RawInputElement)

import Reflex.Material.Types

data MdRadio d t = MdRadio
  { _mdRadio_value :: Dynamic t Bool
  , _mdRadio_change :: Event t Bool
  , _mdRadio_element :: RawInputElement d
  }

instance HasValue (MdRadio d t) where
  type Value (MdRadio d t) = Dynamic t Bool
  value = _mdRadio_value

data MdRadioConfig t = MdRadioConfig
  { _mdRadioConfig_name :: Text
  , _mdRadioConfig_initialValue :: Text
  , _mdRadioConfig_enabled :: Bool
  , _mdRadioConfig_selected :: Bool
  , _mdRadioConfig_setValue :: Event t Bool
  , _mdRadioConfig_attributes :: Dynamic t (Map Text Text)
  }

instance Reflex t => Default (MdRadioConfig t) where
  def = MdRadioConfig "" "" True False never (constDyn mempty)

-- | Radio button -- incomplete. Need to adapt RadioGroup and
-- RadioItem from reflex-dom-semui.
mdRadio :: (DomBuilder t m, DomBuilderSpace m ~ d) => MdRadioConfig t -> m (MdRadio d t)
mdRadio cfg = elClass "div" (toClass cfg) $ do
  let cfg' = InputElementConfig
             { _inputElementConfig_initialValue = _mdRadioConfig_name cfg
             , _inputElementConfig_setValue = Nothing
             , _inputElementConfig_initialChecked = _mdRadioConfig_enabled cfg
             , _inputElementConfig_setChecked = Nothing
             , _inputElementConfig_elementConfig =
               def { _elementConfig_initialAttributes = inputAttrs cfg }
             }
  radio <- inputElement cfg'
  elClass "div" "mdc-radio__background" $ do
    elClass "div" "mdc-radio__outer-circle" blank
    elClass "div" "mdc-radio__inner-circle" blank
  pure $ MdRadio
    { _mdRadio_value = fmap (== _mdRadioConfig_initialValue cfg) (_inputElement_value radio)
    , _mdRadio_change = never -- fixme
    , _mdRadio_element = _inputElement_raw radio
    }

inputAttrs :: Reflex t => MdRadioConfig t -> Map AttributeName Text
inputAttrs cfg = disabledAttr (_mdRadioConfig_enabled cfg) <>
                 selectedAttr (_mdRadioConfig_selected cfg) <>
                 radioAttrs cfg

toClass :: Reflex t => MdRadioConfig t -> Text
toClass cfg | _mdRadioConfig_enabled cfg = "mdc-radio"
            | otherwise = "mdc-radio mdc-radio--disabled"

disabledAttr :: Bool -> Map AttributeName Text
disabledAttr False = "disabled" =: "disabled"
disabledAttr True = mempty

selectedAttr :: Bool -> Map AttributeName Text
selectedAttr True = "checked" =: "checked"
selectedAttr False = mempty

radioAttrs :: Reflex t => MdRadioConfig t -> Map AttributeName Text
radioAttrs MdRadioConfig{..} = "name" =: _mdRadioConfig_name <>
                               "value" =: _mdRadioConfig_initialValue <>
                               "type" =: "radio" <>
                               "class" =: "mdc-radio__native-control"
