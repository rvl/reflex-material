{-# LANGUAGE RecordWildCards #-}

module Reflex.Material.Textfield
  ( mdTextfield
  , MdTextfield(..)
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Default
import Data.Maybe (catMaybes)

import Reflex.Dom

import Reflex.Material.Common
import Reflex.Material.Core
import Reflex.Material.Types
import Reflex.Material.Util

----------------------------------------------------------------------------

data MdTextfield = MdTextfield
    { _mdTextfield_size        :: Maybe MdSize
    , _mdTextfield_disabled    :: Maybe MdDisabled
    -- , _mdTextfield_error       :: Maybe MdError
    , _mdTextfield_dark        :: Maybe MdDark
    , _mdTextfield_custom      :: Maybe Text
    } deriving (Eq,Show)

instance Default MdTextfield where
  def = MdTextfield def def def def

instance MdHasSize MdTextfield where
  mdSetSize s i = i { _mdTextfield_size = Just s }

instance MdHasDisabled MdTextfield where
  disabled i = i { _mdTextfield_disabled = Just MdDisabled }

instance MdHasDark MdTextfield where
  dark i = i { _mdTextfield_dark = Just MdDark }

instance MdHasCustom MdTextfield where
  custom s i = i { _mdTextfield_custom = addCustom s (_mdTextfield_custom i) }

mdTextfieldAttrs :: MdTextfield -> Text
mdTextfieldAttrs MdTextfield{..} = T.unwords $ catMaybes
    [ mdText <$> _mdTextfield_size
    , mdText <$> _mdTextfield_disabled
    , mdText <$> _mdTextfield_dark
    , _mdTextfield_custom
    ]


----------------------------------------------------------------------------

tfClass :: [Text] -> Text
tfClass ts = T.intercalate "__" ("mdc-textfield":ts)

mdTextfield :: MonadWidget t m
             => Dynamic t MdTextfield
             -> TextInputConfig t
             -> Text
             -> m (TextInput t)
mdTextfield _ config label = do
  (el, i) <- elAttr' "div" ("class" =: tfClass []) $ do
    i <- textInput (mdTextInputConfig config)
    let attr = _textInputConfig_attributes config
    let attr' = addClass [tfClass ["label"]] . forId <$> attr
    elDynAttr "label" attr' $ text label
    return i
  elAttr "p" ("id" =: ("fixme" <> "-helptext") <>
              "class" =: "mdc-textfield-helptext" <>
              "aria-hidden" =: "true" <>
              "style" =: "display:none;"
             ) $ text "Help Text (possibly validation message)"
  attachTextfield el
  return i

mdTextInputConfig :: Reflex t => TextInputConfig t -> TextInputConfig t
mdTextInputConfig c@TextInputConfig{..} = c { _textInputConfig_attributes = attrs' }
  where
    attrs' = addClass [tfClass ["input"]] <$> _textInputConfig_attributes
    -- aria i = "aria-controls" =: (i <> "-helptext")


tfPlaceholder :: Map Text Text -> Text
tfPlaceholder = M.findWithDefault "" "placeholder"

{-
<section id="demo-textfield-wrapper">
        <div class="mdc-textfield">
          <input type="text" class="mdc-textfield__input" id="my-textfield"
                 name="email" aria-controls="my-textfield-helptext"
                 data-demo-no-auto-js autocomplete="email">
          <label for="my-textfield" class="mdc-textfield__label">Email Address</label>
        </div>
        <p id="my-textfield-helptext" class="mdc-textfield-helptext"
           aria-hidden="true" style="display:none;">
          Help Text (possibly validation message)
        </p>
      </section>
-}
