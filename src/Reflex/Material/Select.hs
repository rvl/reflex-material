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
import Control.Monad (guard)

import Reflex.Dom

import Reflex.Material.Common
import Reflex.Material.Core
import Reflex.Material.Types
import Reflex.Material.Util

mdSimpleMenu :: MonadWidget t m => m a -> m a
mdSimpleMenu items = elAttr "div" ("class" =: "mdc-simple-menu mdc-select__menu") $
  elAttr "ul" ("class" =: "mdc-list mdc-simple-menu__items") items

-- mdSelect :: forall k t m. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Ord k) => k -> Dynamic t (Map k Text) -> DropdownConfig t k -> m (Dropdown t k)
mdSelect :: (MonadWidget t m, Ord k) => k -> Dynamic t (Map k Text) -> DropdownConfig t k -> m (Dropdown t k)
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

mdSelectElem :: (MonadWidget t m, Ord k) => Dynamic t (Map (Int, k) Text) -> Event t Int -> m (El t, Event t Int)
mdSelectElem indexedOptions setValue = do
  (eRaw, _) <- elDynAttr' "div" (constDyn ("class" =: "mdc-select" <> "role" =: "listbox" <> "tabindex" =: "0")) $ do
    elAttr "span" ("class" =: "mdc-select__selected-text") $ text ""
    mdSimpleMenu $ listWithKey indexedOptions $ \(ix, k) v -> do
      elAttr "li" ("class" =: "mdc-list-item" <> "role" =: "option" <> "tabindex" =: "0") $ dynText v
      return ()
  eSelectChange <- attachSelect (Just setValue) eRaw
  return (eRaw, eSelectChange)

mdSelectOptionText :: MonadWidget t m => Bool -> Text -> Text -> m ()
mdSelectOptionText disabled key label = elAttr "li" attrs $ text label
  where
    attrs = "class" =: "mdc-list-item" <> "role" =: "option" <> dis
    dis = if disabled then "aria-disabled" =: "true" else "tabindex" =: "0"


mdSelectMultiple :: MonadWidget t m => Int -> m a -> m a
mdSelectMultiple size options = elAttr "select" attrs options
  where attrs = "multiple" =: "" <>
                "size" =: (T.pack $ show size) <>
                "class" =: "mdc-multi-select mdc-list"

mdOptGroup :: MonadWidget t m => Text -> m a -> m a
mdOptGroup label opts = elAttr "optgroup" ("class" =: "mdc-list-group" <> "label" =: label) opts

mdOption :: MonadWidget t m => Text -> m ()
mdOption label = elAttr "option" ("class" =: "mdc-list-item") (text label)

mdDivider :: MonadWidget t m => m ()
mdDivider = elAttr "option" ("class" =: "mdc-list-divider" <> "role" =: "presentation" <> "disabled" =: "") blank
