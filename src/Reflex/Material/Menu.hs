{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Reflex.Material.Menu
  ( mdSimpleMenu
  , mdMenuItem
  , mdMenuDivider
  ) where

import Data.Monoid ((<>), mempty)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

import Reflex.Dom

import Reflex.Material.Core


data MdSimpleMenu t k = MdSimpleMenu
  { _mdSimpleMenu_isOpen :: Dynamic t Bool
  , _mdSimpleMenu_change :: Event t Bool
  , _mdSimpleMenu_select :: Event t k
  }

data MdSimpleMenuConfig t k = MdSimpleMenuConfig
  { _mdSimpleMenuConfig_show :: Event t k
  , _mdSimpleMenuConfig_attributes :: Dynamic t (Map Text Text)
  }

mdSimpleMenu :: MonadWidget t m => Event t Bool -> m a -> m (Event t Int)
mdSimpleMenu eOpen items = do
  (el, _) <- elAttr' "div" attrs items
  attachSimpleMenu eOpen el
  where
    attrs = "class" =: "mdc-simple-menu" <>
            "tabindex" =: "-1" <>
            "style" =: "position: absolute;"


mdMenuItem :: MonadWidget t m => Text -> m (Event t ())
mdMenuItem label = do
  (el, _) <- elAttr' "li" attrs $ text label
  return $ () <$ domEvent Click el
  where attrs = "class" =: "mdc-list-item" <>
                "role" =: "menuitem" <>
                "tabindex" =: "0"

mdMenuDivider :: MonadWidget t m => m ()
mdMenuDivider = elAttr "li" ("class" =: "mdc-list-divider" <> "role" =: "separator") blank
