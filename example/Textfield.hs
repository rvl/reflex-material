{-# LANGUAGE OverloadedStrings #-}

module Textfield where

import Data.Monoid ((<>))
import Data.Text (Text)

import Reflex.Dom

import Reflex.Material.Checkbox
import Reflex.Material.Textfield
import Reflex.Material.Typography

textfield :: MonadWidget t m => m ()
textfield = do
  title_ "TextFields"
  display1_ "Full Functionality JS Component (Floating Label, Validation, Autocomplete)"
  let cfg = (def & attributes .~ constDyn ("id" =: "my-textfield"))
  i <- el "div" $ mdTextfield def cfg "Email Address"
  el "div" $ do
    mdCheckboxField False (cbId "cb1") $ text "Disabled"
    mdCheckboxField False (def & attributes .~ constDyn ("id" =: "cb2")) $ text "Dark theme"
    mdCheckboxField False (def & attributes .~ constDyn ("id" =: "cb3")) $ text "Dense"
    mdCheckboxField False (def & attributes .~ constDyn ("id" =: "cb4")) $ text "Required"
    mdCheckboxField False (def & attributes .~ constDyn ("id" =: "cb5")) $ text "Use Helper Text"

  display1_ "Password field with validation"

  display1_ "Multi-line Textfields"

  display1_ "Full-Width Textfields"
  -- let cfg2 = (def & attributes .~ constDyn ("placeholder" =: "Message" <> "id" =: "full-width-textfield"))

cbId :: Reflex t => Text -> CheckboxConfig t
cbId id = def & attributes .~ constDyn ("id" =: id)
