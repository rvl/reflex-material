{-# LANGUAGE RecordWildCards #-}

module Reflex.Material.Icon where

------------------------------------------------------------------------------
import           Data.Default
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as M
import           Reflex.Dom hiding (fromJSString)
------------------------------------------------------------------------------
import           Reflex.Material.Common
import           Reflex.Material.Types
import           Reflex.Material.Util
------------------------------------------------------------------------------

data MdIconToggle = MdIconToggle
    { _mdIconToggle_size       :: Maybe MdSize
    , _mdIconToggle_disabled   :: Maybe MdDisabled
    , _mdIconToggle_dark   :: Maybe MdDark
    } deriving (Eq,Show)

instance Default MdIconToggle where
  def = MdIconToggle def def def

instance MdHasSize MdIconToggle where
  mdSetSize s b = b { _mdIconToggle_size = Just s }

instance MdHasDisabled MdIconToggle where
  disabled b = b { _mdIconToggle_disabled = Just MdDisabled }

instance MdHasDark MdIconToggle where
  dark b = b { _mdIconToggle_dark = Just MdDark }

mdIconToggleAttrs :: MdIconToggle -> Text
mdIconToggleAttrs MdIconToggle{..} = T.unwords $ catMaybes
    [ mdText <$> _mdIconToggle_size
    , mdText <$> _mdIconToggle_disabled
    , mdText <$> _mdIconToggle_dark
    ]

mdIconToggle
    :: DomBuilder t m
    => Text -> Text
    -> Dynamic t MdIconToggle
    -> m (Event t ())
mdIconToggle iconOn iconOff iDyn = do
    (e,_) <- elDynAttr' "i" (mkAttrs <$> iDyn) $ text iconOff

    return $ domEvent Click e
  where
    mkAttrs b = "class" =: T.unwords [mdIconToggleAttrs b, "material-icons"]


----------------------------------------------------------------------------

mdIcon :: DomBuilder t m => Text -> m ()
mdIcon icon = elAttr "i" ("class" =: "material-icons") $ text icon

mdIconAttr :: DomBuilder t m => Map Text Text -> Text -> m ()
mdIconAttr attrs icon = elAttr "i" attrs' $ text icon
  where attrs' =  M.insertWith (<>) "class" " material-icons" attrs

mdIconClass :: DomBuilder t m => Text -> CssClass -> m ()
mdIconClass i c =
  elAttr "i" (  "class"       =: ("material-icons " <> unCssClass c)
             <> "aria-hidden" =: "true"
             ) $ text i

----------------------------------------------------------------------------

faIcon :: DomBuilder t m => Text -> m ()
faIcon icon = elAttr "i" (faIconClass icon M.empty) blank

faIconDynAttr
    :: DomBuilder t m
    => Text
    -> Dynamic t (Map Text Text)
    -> m (Event t ())
faIconDynAttr icon aDyn = do
    (e,_) <- elDynAttr' "i" (faIconClass icon <$> aDyn) blank
    return $ domEvent Click e

faIconClass icon = addClass ["fa", "fa-" <> icon]
