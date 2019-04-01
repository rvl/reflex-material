{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Material.Select
  ( mdSelect
  , Dropdown(..)
  , DropdownConfig(..)
  , mdSelectMultiple
  , mdOptGroup
  , mdOption
  , mdDivider
  ) where


import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import Data.Default
import Control.Monad.Fix
import qualified Text.Read as T
import Data.Maybe (fromMaybe)
import Control.Monad (guard, void)

import Reflex.Dom

import Reflex.Material.Common
import Reflex.Material.Framework (attachSelect, attachMenuSurface)
import Reflex.Material.Types
import Reflex.Material.Util

mdSimpleMenu :: MaterialWidget t m => m a -> m a
mdSimpleMenu items = do
  (elm, a) <- elClass' "div" "mdc-select__menu mdc-menu mdc-menu-surface" $
    elClass "ul" "mdc-list" items
  attachMenuSurface elm
  pure a

mdSelect ::
  ( MaterialWidget t m
  , MonadFix m
  , Ord k)
  => k -> Dynamic t (Map k Text) -> DropdownConfig t k -> m (Dropdown t k)
mdSelect k0 options (DropdownConfig setK attrs) = do
  optionsWithAddedKeys <- fmap (zipDynWith Map.union options) $ foldDyn Map.union (k0 =: "") $ fmap (=: "") setK
  defaultKey <- holdDyn k0 setK
  let (indexedOptions, ixKeys) = splitDynPure $ ffor optionsWithAddedKeys $ \os ->
        let xs = fmap (\(ix, (k, v)) -> ((ix, k), ((ix, k), v))) $ zip [0::Int ..] $ Map.toList os
        in (Map.fromList $ map snd xs, Bimap.fromList $ map fst xs)
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let setValue = attachPromptlyDynWithMaybe (flip Bimap.lookupR) ixKeys setK
  (eRaw, eSelectChange) <- mdSelectElem indexedOptions setValue
  let lookupSelected = flip Bimap.lookup
  let eChange = attachPromptlyDynWith lookupSelected ixKeys $ eSelectChange
  let readKey keys mk = fromMaybe k0 $ do
        k <- mk
        guard $ Bimap.memberR k keys
        return k
  dValue <- fmap (zipDynWith readKey ixKeys) $ holdDyn (Just k0) $ leftmost [eChange, fmap Just setK]
  return $ Dropdown dValue (attachPromptlyDynWith readKey ixKeys eChange)

-- implements MDC Enhanced Select
mdSelectElem
  :: ( MaterialWidget t m, MonadFix m, Ord k)
  => Dynamic t (Map (Int, k) Text)
  -> Event t Int
  -> m (El t, Event t Int)
mdSelectElem indexedOptions setValue = do
  (eRaw, _) <- elDynAttr' "div" (constDyn ("class" =: "mdc-select" <> "role" =: "listbox" <> "tabindex" =: "0")) $ do
    elAttr "input" ("type" =: "hidden" <> "name" =: "enhanced-select") $ blank
    elClass "i" "mdc-select__dropdown-icon" blank
    elClass "div" "mdc-select__selected-text" blank
    mdSimpleMenu $ listWithKey indexedOptions $ \(ix, k) v ->
      let attrs = "class" =: "mdc-list-item" <> "data-value" =: tshow ix <>
                  "role" =: "option" <> "tabindex" =: "0"
      in void $ elAttr "li" attrs $ dynText v
  elClass "span" "mdc-floating-label" $ text "the label goes here"
  elClass "div" "mdc-line-ripple" blank
  eSelectChange <- attachSelect (Just setValue) eRaw
  return (eRaw, eSelectChange)

mdSelectOptionText :: DomBuilder t m => Bool -> Text -> Text -> m ()
mdSelectOptionText disabled key label = elAttr "li" attrs $ text label
  where
    attrs = "class" =: "mdc-list-item" <> "role" =: "option" <> dis
    dis = if disabled then "aria-disabled" =: "true" else "tabindex" =: "0"


mdSelectMultiple :: DomBuilder t m => Int -> m a -> m a
mdSelectMultiple size options = elAttr "select" attrs options
  where attrs = "multiple" =: "" <>
                "size" =: tshow size <>
                "class" =: "mdc-multi-select mdc-list"

mdOptGroup :: DomBuilder t m => Text -> m a -> m a
mdOptGroup label opts = elAttr "optgroup" ("class" =: "mdc-list-group" <> "label" =: label) opts

mdOption :: DomBuilder t m => Text -> m ()
mdOption label = elAttr "option" ("class" =: "mdc-list-item") (text label)

mdDivider :: DomBuilder t m => m ()
mdDivider = elAttr "option" ("class" =: "mdc-list-divider" <> "role" =: "presentation" <> "disabled" =: "") blank
