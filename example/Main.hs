{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Data.Text (Text)
import Reflex.Dom

import Reflex.Material.Basic
import Reflex.Material.Card
import Reflex.Material.Drawer
import Reflex.Material.List
import Reflex.Material.Toolbar
import Reflex.Material.Types

import Buttons
import Cards
import Checkbox
import Fab
import LayoutGrid
import Lists
import Textfield
import Toolbar
import Typography

main :: IO ()
main = mainWidgetWithHead head_ body_

head_ :: DomBuilder t m => m ()
head_ = do
  el "title" $ text "Example Reflex-Material"
  mobile_
  styles_ defaultStyle
  stylesheet_ "css/example.css"

nav :: MonadWidget t m => m (Event t (m ()))
nav = do
  (b1, _) <- btn "Buttons"
  (b2, _) <- btn "Cards"
  (b3, _) <- btn "Checkbox"
  (b4, _) <- btn "Layout Grid"
  (b5, _) <- btn "List"
  (b6, _) <- btn "Text Field"
  (b7, _) <- btn "Typography"
  (b8, _) <- btn "Fab"
  pure $ leftmost [ buttons    <$ domEvent Click b1
                  , cards      <$ domEvent Click b2
                  , checkboxEx <$ domEvent Click b3
                  , layoutGrid <$ domEvent Click b4
                  , lists      <$ domEvent Click b5
                  , textfield  <$ domEvent Click b6
                  , typography <$ domEvent Click b7
                  , fab        <$ domEvent Click b8
                  ]
  where
    btn :: MonadWidget t m => Text -> m (El t, ())
    btn t = do
      (e, _) <- elAttr' "span" mempty $ item_ "a" mempty $ do
        icon_ "lens" mdcListItemStartDetail_
        text t
      pure (e, ())

body_ :: MonadWidget t m => m ()
body_ = do
  toolbar
  elClass "div" (unCssClass $ CssClass "demo-content" <> mdcToolbarFixedAdjust_) $ do
    v <- drawer_ mdcPermanentDrawer_ $ drawerContent_ $ list_ "nav" mempty nav
    main_ (CssClass "demo-main") $ widgetHold buttons v
