{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections #-}

module Reflex.Material.Textfield
  ( mdTextfield
  , mdTextfieldMulti
  , mdTextfieldHelpText
  , MdTextfield(..)
  , MdFullwidth(..)
  , MdHasFullwidth(..)
  , MdHelpTextConfig(..)
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
import Reflex.Material.Core
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
mdTextfieldClass = "mdc-textfield"

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

mdTextfield :: MonadWidget t m
            => Dynamic t MdTextfield
            -> TextInputConfig t
            -> Text
            -> m (TextInput t)
mdTextfield md config label = do
  (el, i) <- mdTextfieldContainer False md $ do
    i <- textInput (mdTextInputConfig config)
    let attr = _textInputConfig_attributes config
    let attr' = addClass [tfPart ["label"]] . forId <$> attr
    elDynAttr "label" attr' $ text label
    return i
  attachTextfield el
  return i

mdTextfieldContainer :: MonadWidget t m => Bool -> Dynamic t MdTextfield -> m a -> m (El t, a)
mdTextfieldContainer multi dynConfig = elDynAttr' "div" (clsAttr <$> dynConfig)
  where
    -- note that the mdc --upgraded style needs to be preserved
    clsAttr MdTextfield{..} = "class" =: cls [ Just ""
                                             , Just "upgraded"
                                             , mwhen multi (Just "multiline")
                                             , mdText <$> _mdTextfield_density
                                             , mdText <$> _mdTextfield_fullwidth
                                             ]
    cls = T.unwords . map tfClass . catMaybes


data MdHelpTextConfig = MdHelpTextConfig
    { _mdHelpText_shown         :: Bool
    , _mdHelpText_persistent    :: Bool
    , _mdHelpText_validationMsg :: Bool
    } deriving (Eq,Show)

instance Default MdHelpTextConfig where
  def = MdHelpTextConfig True False False

helpTextPersistent :: MdHelpTextConfig -> MdHelpTextConfig
helpTextPersistent c = c { _mdHelpText_persistent = True }

helpTextValidationMsg :: MdHelpTextConfig -> MdHelpTextConfig
helpTextValidationMsg c = c { _mdHelpText_validationMsg = True }

mdTextfieldHelpText :: MonadWidget t m
                    => TextInputConfig t
                    -> Dynamic t MdHelpTextConfig
                    -> m a -> m a
mdTextfieldHelpText input config = elDynAttr "p" (update <$> input ^. attributes <*> config)
  where
    update attrs MdHelpTextConfig{..} =
      idSuffix "-helptext" attrs <>
      "class" =: T.unwords (cls _mdHelpText_persistent _mdHelpText_validationMsg) <>
      mwhen (not _mdHelpText_shown) ("style" =: "display: none;" <> "aria-hidden" =: "true")
    cls p v = map htClass ("" : mwhen p ["persistent"] ++ mwhen v ["validation-msg"])
    htClass "" = mdTextfieldClass <> "-helptext"
    htClass s = htClass "" <> "--" <> s


mdTextfieldHelpTextOld :: MonadWidget t m
                       => TextInputConfig t
                       -> Dynamic t Bool
                       -> Dynamic t Bool
                       -> Dynamic t Bool
                       -> m a
                       -> m a
mdTextfieldHelpTextOld config shown persistent validation contents = elDynAttr "p" dynAttrs contents
  where
    dynAttrs = update <$> _textInputConfig_attributes config <*> shown <*> persistent <*> validation
    update attrs s p v = idSuffix "-helptext" attrs <>
                            "class" =: T.unwords (cls p v) <>
                            mwhen (not s) ("style" =: "display: none;" <> "aria-hidden" =: "true")
    cls p v = map htClass ("" : mwhen p ["persistent"] ++ mwhen v ["validation-msg"])
    htClass "" = mdTextfieldClass <> "-helptext"
    htClass s = htClass "" <> "--" <> s

mdTextInputConfig :: Reflex t => TextInputConfig t -> TextInputConfig t
mdTextInputConfig c@TextInputConfig{..} =
  c { _textInputConfig_attributes = mdTextFieldInputAttrs <$> _textInputConfig_attributes }

mdTextFieldInputAttrs :: Map Text Text -> Map Text Text
mdTextFieldInputAttrs attrs = addClass [tfPart ["input"]] attrs
    -- aria i = "aria-controls" =: (i <> "-helptext")

mdTextAreaConfig :: Reflex t => TextAreaConfig t -> TextAreaConfig t
mdTextAreaConfig = over attributes (fmap mdTextFieldInputAttrs)

mwhen :: Monoid a => Bool -> a -> a
mwhen p a = if p then a else mempty

tfPlaceholder :: Map Text Text -> Text
tfPlaceholder = M.findWithDefault "" "placeholder"

mdTextfieldMulti :: MonadWidget t m
                 => Dynamic t MdTextfield
                 -> TextAreaConfig t
                 -> Text
                 -> m (TextArea t)
mdTextfieldMulti md config label = do
  (el, i) <- mdTextfieldContainer True md $ do
    i <- textArea (mdTextAreaConfig config)
    let attr = config ^. attributes
    let attr' = addClass [tfPart ["label"]] . forId <$> attr
    elDynAttr "label" attr' $ text label
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
