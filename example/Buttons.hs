{-# LANGUAGE OverloadedStrings #-}

module Buttons where

import Data.Monoid ((<>))
import Reflex.Dom

import Reflex.Material.Button
import Reflex.Material.Typography
import Reflex.Material.Common

buttons :: MonadWidget t m => m ()
buttons = do
  title_ "Buttons"
  el "div" $ do
    display2_ "With javascript"
    theButtons

  el "div" $ do
    display2_ "Disabled"
    elAttr "fieldset" ("disabled" =: "disabled") $ do
      el "legend" $ text "Disabled buttons"
      theButtons

  divClass "mdc-theme--dark" $ do
    display1_ "Dark theme"
    theButtons

theButtons :: MonadWidget t m => m ()
theButtons = divClass "demo-buttons" $ do
  click1 <- mdButton (pure def) $ text "Default"
  mdButton (def & raised) $ text "Raised"
  mdButton (def & dense) $ text "Dense Default"
  mdButton (def & dense & raised) $ text "Dense Raised"
  mdButton (def & primary) $ text "Default Primary"
  mdButton (def & primary & raised) $ text "Primary Raised"
  mdButton (def & primary & dense) $ text "Primary Dense"
  mdButton (def & accent) $ text "Default Accent"
  mdButton (def & accent & raised) $ text "Accent Raised"
  mdButton (def & accent & dense) $ text "Accent Dense"
  return ()
