{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Material.Fab
  ( mdFab
  , mdFabIcon
  , MdFab(..)
  , MdBackground
  , MdHasBackground(..)
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import           Data.Default
import           Data.Maybe
import Data.Map (Map)
import Control.Lens (makeLenses, lens)

import Reflex.Dom

import Reflex.Material.Types
import Reflex.Material.Common
import Reflex.Material.Framework (attachRipple)

----------------------------------------------------------------------------

data MdBackground = MdPlain
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance MdClassText MdBackground where
  mdText MdPlain = "plain"

class MdHasBackground a where
  plain :: a -> a

instance (Reflex t, MdHasBackground a) => MdHasBackground (Dynamic t a) where
  plain = fmap plain

------------------------------------------------------------------------------
data MdFab = MdFab
    { _mdFab_background :: Maybe MdBackground
    , _mdFab_size       :: Maybe MdSize
    , _mdFab_custom     :: Maybe Text
    , _mdFab_attributes :: Map Text Text
    }

makeLenses ''MdFab

instance Default MdFab where
  def = MdFab def def def def

instance MdHasSize MdFab where
  mdSetSize c b = b { _mdFab_size = Just c }

instance MdHasBackground MdFab where
  plain b = b { _mdFab_background = Just MdPlain }

instance MdHasCustom MdFab where
  custom s i = i { _mdFab_custom = addCustom s (_mdFab_custom i) }

instance HasAttributes MdFab where
  type Attrs MdFab = Map Text Text
  -- attributes = lens _mdFab_attributes (\fab attrs -> fab { _mdFab_attributes = attrs })
  attributes = mdFab_attributes

------------------------------------------------------------------------------

mdFabClass :: MdFab -> Text
mdFabClass MdFab{..} = T.unwords ("mdc-fab":cs) <> custom
  where
    cs = map ("mdc-fab--" <>) $ catMaybes
         [ mdText <$> _mdFab_background
         , mdText <$> _mdFab_size
         ]
    custom = fromMaybe "" _mdFab_custom

mdFab :: MonadWidget t m
      => Dynamic t MdFab
      -> m ()
      -> m (Event t ())
mdFab fabDyn contents = do
  (e, _) <- elDynAttr' "button" (mdFabAttrs "" "" <$> fabDyn) $
            elAttr "span" ("class" =: "mdc-fab__icon") contents
  attachRipple e
  return $ domEvent Click e

mdFabIcon :: MonadWidget t m
          => Dynamic t MdFab
          -> Text
          -> Text
          -> m (Event t ())
mdFabIcon fabDyn icon label = do
  (e, _) <- elDynAttr' "button" (mdFabAttrs " material-icons" label <$> fabDyn) $
            elAttr "span" ("class" =: "mdc-fab__icon") $ text icon
  attachRipple e
  return $ domEvent Click e

mdFabAttrs :: Text -> Text -> MdFab -> Map Text Text
mdFabAttrs cls label fab =
  _mdFab_attributes fab <>
  "class" =: (mdFabClass fab <> cls) <>
  "aria-label" =: label
