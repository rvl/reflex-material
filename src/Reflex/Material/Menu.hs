{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Reflex.Material.Menu
  ( mdMenu
  , mdMenuItem
  , mdMenuDivider
  ) where

import Data.Monoid ((<>), mempty)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

import Reflex.Dom

import Reflex.Material.Common (MaterialWidget)
import Reflex.Material.Framework (attachMenu)


data MdMenu t k = MdMenu
  { _mdMenu_isOpen :: Dynamic t Bool
  , _mdMenu_change :: Event t Bool
  , _mdMenu_select :: Event t k
  }

data MdMenuConfig t k = MdMenuConfig
  { _mdMenuConfig_show :: Event t k
  , _mdMenuConfig_attributes :: Dynamic t (Map Text Text)
  }

mdMenu :: MaterialWidget t m => Event t Bool -> m a -> m (Event t Int)
mdMenu eOpen items = do
  (el, _) <- elAttr' "div" attrs items
  attachMenu eOpen el
  where
    attrs = "class" =: "mdc-menu mdc-menu-surface" <>
            "tabindex" =: "-1" <>
            "style" =: "position: absolute;"


mdMenuItem :: DomBuilder t m => Text -> m (Event t ())
mdMenuItem label = do
  (el, _) <- elAttr' "li" attrs $ text label
  return $ () <$ domEvent Click el
  where attrs = "class" =: "mdc-list-item" <>
                "role" =: "menuitem" <>
                "tabindex" =: "0"

mdMenuDivider :: DomBuilder t m => m ()
mdMenuDivider = elAttr "li" ("class" =: "mdc-list-divider" <> "role" =: "separator") blank
