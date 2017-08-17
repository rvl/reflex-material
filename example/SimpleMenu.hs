{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module SimpleMenu (simpleMenuEx) where

import Data.Monoid ((<>), mempty)
import Reflex.Dom
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

import Reflex.Material.Button
import Reflex.Material.Checkbox
import Reflex.Material.Menu
import Reflex.Material.Typography
import Reflex.Material.Common

top, left, bottom, right :: Map Text Text
top = ("top" =: "0")
left = ("left" =: "0")
bottom = ("bottom" =: "0")
right = ("right" =: "0")

simpleMenuEx :: MonadWidget t m => m ()
simpleMenuEx = do
  elAttr "main" ("class" =: "simple-menu") $ do
    title_ "MDC Simple Menu"
    alignStyle <- el "div" $ do
      let group = "g"
      let dynAttrs = constDyn mempty
      evRad1 <- radioBtn "Top left" group (top <> left) dynAttrs
      evRad2 <- radioBtn "Top right" group (top <> right) dynAttrs
      evRad3 <- radioBtn "Bottom right" group (bottom <> right) dynAttrs
      evRad4 <- radioBtn "Bottom left" group (bottom <> left) dynAttrs
      let evRadio = leftmost [evRad1, evRad2, evRad3, evRad4]
      holdDyn mempty (fmap (<> "position" =: "absolute") evRadio)

    rec
      el "div" $ do
        text "Last selected item: "
        el "i" $ text "<none selected>"

      eMenu <- divClass "demo-content" $ do
         mdMenuAnchor alignStyle $ do
           eToggle <- mdButton def (text "Toggle")

           eMenu <- mdSimpleMenu (True <$ eToggle) $
             elAttr "ul" ("class" =: "mdc-simple-menu__items mdc-list" <>
                          "role" =: "menu" <>
                          "aria-hidden" =: "true") $ do
             let item t = fmap (const t) <$> mdMenuItem t
             b1 <- item "Back"
             b2 <- item "Forward"
             mdMenuItem "Reload"
             mdMenuDivider
             mdMenuItem "Save As..."
             mdMenuItem "Print..."
             mdMenuItem "Cast..."
             mdMenuItem "Translate to English"
             mdMenuDivider
             mdMenuItem "View Page Source"
             mdMenuItem "Inspect"
             return $ leftmost [b1, b2]
           return eMenu
    return ()

mdMenuAnchor :: MonadWidget t m => Dynamic t (Map Text Text) -> m a -> m a
mdMenuAnchor dynStyle = elDynAttr "div" (attrs <$> dynStyle)
  where
    attrs st = "class" =: "mdc-menu-anchor" <> "style" =: styles st
    styles st = T.intercalate "; " [k <> ": " <> v | (k, v) <- M.toList st]

-- | Helper function to create a radio button
radioBtn :: (Eq a, Show a, MonadWidget t m) => Text -> Text -> a -> Dynamic t (Map Text Text)-> m (Event t a)
radioBtn label group rid dynAttrs = do
  el "br" blank
  ev <- elDynAttr "label" dynAttrs $ do
    (rb1, _) <- elAttr' "input" ("name" =: group <> "type" =: "radio" <> "value" =: T.pack (show rid)) blank
    text label
    return $ domEvent Click rb1
  return $ rid <$ ev


cbex :: MonadWidget t m => Text -> Text -> m (Dynamic t Bool)
cbex i t = _checkbox_value <$> el "div" field
  where
    field = mdCheckboxField False (def & attributes .~ attrs) (text t)
    attrs = constDyn ("id" =: ("cb" <> i))
