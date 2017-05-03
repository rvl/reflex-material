{-# LANGUAGE RecordWildCards #-}

module Reflex.Material.Button
  ( mdButton
  , MdButton(..)
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import           Data.Default
import           Data.Maybe
import Data.Map (Map)

import Reflex.Dom

import Reflex.Material.Types
import Reflex.Material.Common
import Reflex.Material.Framework (attachRipple)

------------------------------------------------------------------------------
-- | Data structure describing options available for buttons.  The typical way
-- of using this data structure is to use the default instance and modify it
-- using the various MdHasXYZ type classes.  For instance:
--
-- @huge $ inverted $ blue def@
data MdButton = MdButton
    { _mdButton_role       :: Maybe MdRole
    , _mdButton_raised     :: Maybe MdRaised
    , _mdButton_activation :: Maybe (Either MdActive MdDisabled)
    -- ^ active and disabled should be mutually exclusive so we use an Either
    , _mdButton_size       :: Maybe MdSize
    , _mdButton_density    :: Maybe MdDensity
    , _mdButton_custom     :: Maybe Text
    } deriving (Eq,Show)

instance Default MdButton where
  def = MdButton def def def def def def

instance MdHasRole MdButton where
  mdSetRole e b = b { _mdButton_role = Just e }

instance MdHasRaised MdButton where
  raised b = b { _mdButton_raised = Just MdRaised }

instance MdHasActive MdButton where
  active b = b { _mdButton_activation = Just $ Left MdActive }

instance MdHasDisabled MdButton where
  disabled b = b { _mdButton_activation = Just $ Right MdDisabled }

instance MdHasSize MdButton where
  mdSetSize c b = b { _mdButton_size = Just c }

instance MdHasDensity MdButton where
  mdSetDensity d b = b { _mdButton_density = Just d }

instance MdHasCustom MdButton where
  custom s i = i { _mdButton_custom = addCustom s (_mdButton_custom i) }

------------------------------------------------------------------------------
-- | Helper function mostly intended for internal use.  Exported for
-- completeness.
mdButtonClass :: MdButton -> Text
mdButtonClass MdButton{..} = T.unwords ("mdc-button":cs) <> custom
  where
    cs = map ("mdc-button--" <>) $ catMaybes
         [ mdText <$> _mdButton_role
         , mdText <$> _mdButton_raised
         , mdText <$> _mdButton_activation
         , mdText <$> _mdButton_size
         , mdText <$> _mdButton_density
         ]
    custom = fromMaybe "" _mdButton_custom

----------------------------------------------------------------------------

-- | Buttons are for clicking
mdButton :: MonadWidget t m
         => Dynamic t MdButton
         -> m ()
         -> m (Event t ())
mdButton bDyn children = do
  (e, _) <- elDynAttr' "button" (mkAttrs <$> bDyn) children
  attachRipple e
  return $ domEvent Click e
  where
    mkAttrs :: MdButton -> Map Text Text
    mkAttrs b = "class" =: T.unwords ["mdc-button", mdButtonClass b, "button"]
