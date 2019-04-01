{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Material.Textfield
  ( mdTextfield
  , mdTextFieldMulti
  , mdTextFieldHelperText
  , MdTextfield(..)
  , MdFullwidth(..)
  , MdHasFullwidth(..)
  , MdHelperTextConfig(..)
  , helpTextValidationMsg
  , helpTextPersistent
  , placeholder
  , textAreaConfig
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Default
import Data.Maybe (catMaybes, maybeToList)
import Control.Lens ((%~), (^.), over)
import Reflex.Dom

import Reflex.Material.Common
import Reflex.Material.Framework (attachTextfield, attachLineRipple, attachFloatingLabel)
import Reflex.Material.Types
import Reflex.Material.Util

----------------------------------------------------------------------------

data MdFullwidth = MdFullwidth
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance MdClassText MdFullwidth where
  mdText MdFullwidth = "fullwidth"

class MdHasFullwidth a where
  fullwidth :: a -> a

instance (Reflex t, MdHasFullwidth a) => MdHasFullwidth (Dynamic t a) where
  fullwidth = fmap fullwidth

----------------------------------------------------------------------------

data MdTextfield = MdTextfield
    { _mdTextfield_size        :: Maybe MdSize
    , _mdTextfield_disabled    :: Maybe MdDisabled
    , _mdTextfield_density     :: Maybe MdDensity
    -- , _mdTextfield_error       :: Maybe MdError
    , _mdTextfield_dark        :: Maybe MdDark
    , _mdTextfield_fullwidth   :: Maybe MdFullwidth
    , _mdTextfield_custom      :: Maybe Text
    } deriving (Eq,Show)

instance Default MdTextfield where
  def = MdTextfield def def def def def def

instance MdHasSize MdTextfield where
  mdSetSize s i = i { _mdTextfield_size = Just s }

instance MdHasDisabled MdTextfield where
  disabled i = i { _mdTextfield_disabled = Just MdDisabled }

instance MdHasDensity MdTextfield where
  mdSetDensity s i = i { _mdTextfield_density = Just s }

instance MdHasDark MdTextfield where
  dark i = i { _mdTextfield_dark = Just MdDark }

instance MdHasFullwidth MdTextfield where
  fullwidth i = i { _mdTextfield_fullwidth = Just MdFullwidth }

instance MdHasCustom MdTextfield where
  custom s i = i { _mdTextfield_custom = addCustom s (_mdTextfield_custom i) }

mdTextfieldAttrs :: MdTextfield -> Text
mdTextfieldAttrs MdTextfield{..} = T.unwords $ catMaybes
    [ mdText <$> _mdTextfield_size
    , mdText <$> _mdTextfield_disabled
    , mdText <$> _mdTextfield_density
    , mdText <$> _mdTextfield_dark
    , _mdTextfield_custom
    ]


----------------------------------------------------------------------------

mdTextfieldClass :: Text
mdTextfieldClass = "mdc-text-field"

-- | Internal parts of the component are usually labeled with these
-- css classes, but it doesn't seem to be consistent.
tfPart :: [Text] -> Text
tfPart ts = T.intercalate "__" (mdTextfieldClass:ts)

-- | The component container element can receive special style
-- classes.
tfClass :: Text -> Text
tfClass "" = mdTextfieldClass
tfClass name = mdTextfieldClass <> "--" <> name

----------------------------------------------------------------------------

mdLineRipple :: MaterialWidget t m => m ()
mdLineRipple = do
  (elm, _) <- elClass' "div" "mdc-line-ripple" blank
  attachLineRipple elm

mdFloatingLabel :: MaterialWidget t m => Text -> Dynamic t (Map Text Text) -> m ()
mdFloatingLabel label attr = do
  let labelAttr = addClass ["mdc-floating-label"] . forId <$> attr
  (elm, _) <- elDynAttr' "label" labelAttr $ text label
  attachFloatingLabel elm

mdTextfield :: (MaterialWidget t m, PostBuild t m)
            => Dynamic t MdTextfield
            -> TextInputConfig t
            -> Text
            -> m (TextInput t)
mdTextfield md config label = do
  (el, i) <- mdTextFieldContainer False md $ do
    i <- textInput (mdTextInputConfig config)
    mdFloatingLabel label (config ^. attributes)
    mdLineRipple
    return i
  attachTextfield el
  return i

mdTextFieldContainer
  :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, PostBuild t m)
  => Bool -> Dynamic t MdTextfield -> m a -> m (El t, a)
mdTextFieldContainer multi dynConfig = elDynAttr' "div" (clsAttr <$> dynConfig)
  where
    -- note that the mdc --upgraded style needs to be preserved
    clsAttr MdTextfield{..} = "class" =: cls [ Just ""
                                             , Just "upgraded"
                                             , optional multi (Just "textarea")
                                             , mdText <$> _mdTextfield_density
                                             , mdText <$> _mdTextfield_fullwidth
                                             ]
    cls = T.unwords . map tfClass . catMaybes


data MdHelperTextConfig = MdHelperTextConfig
    { _mdHelperText_shown         :: Bool
    , _mdHelperText_persistent    :: Bool
    , _mdHelperText_validationMsg :: Bool
    } deriving (Eq,Show)

instance Default MdHelperTextConfig where
  def = MdHelperTextConfig True False False

helpTextPersistent :: MdHelperTextConfig -> MdHelperTextConfig
helpTextPersistent c = c { _mdHelperText_persistent = True }

helpTextValidationMsg :: MdHelperTextConfig -> MdHelperTextConfig
helpTextValidationMsg c = c { _mdHelperText_validationMsg = True }

mdTextFieldHelperLine :: DomBuilder t m => m a -> m a
mdTextFieldHelperLine = elClass "div" (mdTextfieldClass <> "-helper-line")
mdTextFieldHelperText
  :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, PostBuild t m)
  => TextInputConfig t
  -> Dynamic t MdHelperTextConfig
  -> m a -> m a
mdTextFieldHelperText input config = mdTextFieldHelperLine .
  elDynAttr "div" (update <$> input ^. attributes <*> config)
  where
    update attrs MdHelperTextConfig{..} =
      idSuffix "-helptext" attrs <>
      "class" =: T.unwords (cls _mdHelperText_persistent _mdHelperText_validationMsg) <>
      optional (not _mdHelperText_shown) ("style" =: "display: none;" <> "aria-hidden" =: "true")
    cls p v = map htClass ("" : optional p ["persistent"] ++ optional v ["validation-msg"])
    htClass "" = mdTextfieldClass <> "-helper-text"
    htClass s = htClass "" <> "--" <> s
    -- fixme: need to instantiate javascript?

mdTextInputConfig :: Reflex t => TextInputConfig t -> TextInputConfig t
mdTextInputConfig c@TextInputConfig{..} =
  c { _textInputConfig_attributes = mdTextFieldInputAttrs <$> _textInputConfig_attributes }

mdTextFieldInputAttrs :: Map Text Text -> Map Text Text
mdTextFieldInputAttrs attrs = addClass [tfPart ["input"]] attrs
    -- aria i = "aria-controls" =: (i <> "-helptext")

mdTextAreaConfig :: Reflex t => TextAreaConfig t -> TextAreaConfig t
mdTextAreaConfig = over attributes (fmap mdTextFieldInputAttrs)

optional :: Monoid a => Bool -> a -> a
optional p a = if p then a else mempty

tfPlaceholder :: Map Text Text -> Text
tfPlaceholder = M.findWithDefault "" "placeholder"

mdTextFieldMulti
  :: (MaterialWidget t m, PostBuild t m)
  => Dynamic t MdTextfield
  -> TextAreaConfig t
  -> Text
  -> m (TextArea t)
mdTextFieldMulti md config label = do
  (el, i) <- mdTextFieldContainer True md $ do
    i <- textArea (mdTextAreaConfig config)
    mdFloatingLabel label (config ^. attributes(
    mdLineRipple
    return i
  attachTextfield el
  return i

-- placeholder :: (Reflex t, HasAttributes (cfg t)) => Text -> cfg t -> cfg t
placeholder :: Reflex t => Text -> TextInputConfig t -> TextInputConfig t
placeholder t = over attributes (fmap (<> "placeholder" =: t))

textAreaConfig :: Reflex t => Maybe Text -> Maybe Int -> Maybe Int -> TextAreaConfig t
textAreaConfig p rows cols = def & attributes .~ (constDyn . M.fromList $ catMaybes attrs)
  where attrs = [ fmap ("placeholder",) p
                , fmap (("rows",) . tshow) rows
                , fmap (("cols",) . tshow) cols
                ]
