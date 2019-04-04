{-# LANGUAGE OverloadedStrings #-}

module TopAppBar (topAppBar) where

import Data.Text (Text)
import Reflex.Dom
import Control.Monad.Fix (MonadFix)

import Reflex.Material.TopAppBar
import Reflex.Material.Icon

topAppBar :: (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) => Dynamic t (Maybe Text) -> m (Event t ())
topAppBar = topAppBar_ Fixed AlignStart Nothing . (>>= switchHoldPromptly never) . dyn . fmap topAppBarContent

topAppBarContent :: DomBuilder t m => Maybe Text -> m (Event t ())
topAppBarContent Nothing = do
  elClass "span" "catalog-logo mdc-top-app-bar__icon--menu" $
    elAttr "img" ("src" =: "images/ic_component_24px_white.svg") blank
  topAppBarTitle_ "Reflex-Material Catalog"
  pure never
topAppBarContent (Just text) = do
  (e, _) <- elClass' "a" "catalog-logo mdc-top-app-bar__icon--menu" (mdIcon "arrow_back")
  topAppBarTitle_ text
  pure (domEvent Click e)
