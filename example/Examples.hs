{-# LANGUAGE OverloadedStrings, GADTs, ExistentialQuantification #-}

module Examples (head_, body_) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Control.Monad (forM, join)
import Data.Maybe (fromMaybe, isNothing)

import Reflex.Dom

import Reflex.Material.Basic
import Reflex.Material.Button
import Reflex.Material.Card
import Reflex.Material.Drawer
import Reflex.Material.List
import Reflex.Material.Toolbar
import Reflex.Material.Types

import Buttons
import Cards
import Checkbox
import Elevation
import Fab
import LayoutGrid
import Lists
import Ripple
import SimpleMenu
import Select
import Textfield
import Toolbar
import Typography
import Styles

head_ :: DomBuilder t m => m ()
head_ = do
  el "title" $ text "Reflex-Material Catalog"
  mobile_
  styles_ defaultStyle
  mdcScript
  el "style" (text exampleCss)
  stylesheet_ "css/example.css"

examples :: forall t m. DomBuilder t m => [(Text, Text, Text, Maybe (m ()))]
examples = [ ("Button", "Raised and flat buttons", "button", Just buttonEx)
           , ("Card", "Various card layout styles", "card", Just cardEx)
           , ("Checkbox", "Multi-selection controls", "selection_control", Just checkboxEx)
           , ("Dialog", "Secondary text", "dialog", Nothing)
           , ("Drawer", "Temporary", "side_navigation", Nothing)
           , ("Drawer", "Persistent", "side_navigation", Nothing)
           , ("Drawer", "Permanent drawer above toolbar", "side_navigation", Nothing)
           , ("Drawer", "Permanent drawer below toolbar", "side_navigation", Nothing)
           , ("Elevation", "Shadow for different elevations", "shadow", Just elevationEx)
           , ("Floating action button", "The primary action in an application", "button", (Just fabEx))
           , ("Grid list", "2D grid layouts", "card", Nothing)
           , ("Icon toggle", "Toggling icon states", "component", Nothing)
           , ("Layout grid", "Grid and gutter support", "card", Just layoutGridEx)
           , ("Linear progress", "Fills from 0% to 100%, represented by bars", "progress_activity", Nothing)
           , ("List", "Item layouts in lists", "list", Just listEx)
           , ("Radio buttons", "Single selection controls", "radio_button", Nothing)
           , ("Ripple", "Touch ripple", "ripple", Just rippleEx)
           , ("Select", "Popover selection menus", "menu", Just selectEx)
           , ("Simple Menu", "Pop over menus", "menu", Just simpleMenuEx)
           , ("Slider", "Range Controls", "slider", Nothing)
           , ("Snackbar", "Transient messages", "toast", Nothing)
           , ("Switch", "On off switches", "switch", Nothing)
           , ("Tabs", "Tabs with support for icon and text labels", "tabs", Nothing)
           , ("Text field", "Single and multiline text fields", "text_field", Just textfieldEx)
           , ("Theme", "Using primary and accent colors", "theme", Nothing)
           , ("Toolbar", "Header and footers", "toolbar", Nothing)
           , ("Typography", "Type hierarchy", "typography", Just typographyEx)
          ]

nav :: DomBuilder t m => m (Event t (m ()))
nav = do
  btns <- forM examples $ \(title, desc, icon, ex) -> do
    click <- exampleBtn title desc icon (isNothing ex)
    pure (fromMaybe todoEx ex <$ click)
  pure $ leftmost btns

exampleMenu :: DomBuilder t m => m (Event t (Maybe Text, m ()))
exampleMenu = list_ "ul" (mdcListTwoLine_ <> CssClass "catalog-list") $ do
  btns <- forM examples $ \(title, desc, icon, ex) -> do
    click <- exampleBtn title desc icon (isNothing ex)
    pure ((Just title, fromMaybe todoEx ex) <$ click)
  pure $ leftmost btns

exampleBtn :: DomBuilder t m => Text -> Text -> Text -> Bool -> m (Event t ())
exampleBtn title desc icon todo = do
  (e, _) <- elClass' "li" (unCssClass mdcListItem_ <> (if todo then " demo-todo" else "")) $ do
    elClass "span" ("catalog-list-icon " <> unCssClass mdcListItemStartDetail_) (iconEx icon)
    elAttr "a" ("class" =: (unCssClass mdcListItemText_) <> "href" =: "") $ do
      elClass "span" (unCssClass mdcListItemTextPrimary_) (text title)
      elClass "span" (unCssClass mdcListItemTextSecondary_) (text desc)
      return ()
  mdLinkClickEvent e

iconEx name = elAttr "img" ("class" =: "catalog-component-icon" <> "src" =: src) blank
  where src = "images/ic_" <> name <> "_24px.svg"

body_ :: forall t m. DomBuilder t m => m ()
body_ = mdo
  titleDyn <- holdDyn Nothing titleEv
  backEv <- toolbar titleDyn
  titleEv <- elClass "div" (unCssClass $ CssClass "demo-content" <> mdcToolbarFixedAdjust_) $ do
    -- v <- drawer_ mdcPermanentDrawer_ $ drawerContent_ $ list_ "nav" mdcListTwoLine_ nav
    menuEv <- exampleMenu
    let menuEv' = leftmost [menuEv, (Nothing, blank) <$ backEv]
    main_ (CssClass "demo-main") $ do
      widgetHold blank (snd <$> menuEv')
    pure (fst <$> menuEv')
  pure ()

todoEx :: DomBuilder t m => m ()
todoEx = text "Not implemented yet!"
