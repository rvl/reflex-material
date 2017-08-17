{-# LANGUAGE OverloadedStrings #-}

module Toolbar (toolbar) where

import Data.Text (Text)
import Reflex.Dom

import Reflex.Material.Toolbar
import Reflex.Material.Icon

toolbar :: MonadWidget t m => Dynamic t (Maybe Text) -> m (Event t ())
toolbar = toolbar_ Fixed AlignStart Nothing . (>>= switchPromptly never) . dyn . fmap toolbarContent

toolbarContent :: MonadWidget t m => Maybe Text -> m (Event t ())
toolbarContent Nothing = do
  elClass "span" "catalog-logo mdc-toolbar__icon--menu" $
    elAttr "img" ("src" =: "images/ic_component_24px_white.svg") blank
  toolbarTitle_ "Reflex-Material Catalog"
  pure never
toolbarContent (Just text) = do
  (e, _) <- elClass' "a" "catalog-logo mdc-toolbar__icon--menu" (mdIcon "arrow_back")
  toolbarTitle_ text
  pure (domEvent Click e)
