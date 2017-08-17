module Textfield (textfieldEx) where

import Data.Monoid ((<>), mempty)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Text as T
import Control.Lens (over)

import Reflex.Dom

import Reflex.Material.Checkbox
import Reflex.Material.Textfield
import Reflex.Material.Typography
import Reflex.Material.Common

textfieldEx :: MonadWidget t m => m ()
textfieldEx = do
  title_ "TextFields"
  display1_ "Full Functionality JS Component (Floating Label, Validation, Autocomplete)"

  let always = constDyn True
  rec
    divTheme dark rtl $ do
      let config = def & attributes .~ attrs
      mdTextfield tf config "Email Address"
      mdTextfieldHelpText config helper $
        text "Help Text (possibly validation message)"

    (dark, rtl, tf, attrs, helper) <- el "div" $ do
      disabled <- cbex 1 always "Disabled"
      rtl      <- cbex 2 always "RTL"
      dark     <- cbex 3 always "Dark theme"
      dense    <- cbex 4 always "Dense"
      required <- cbex 5 always "Required"
      helper   <- cbex 6 always "Use Helper Text"
      helperP  <- cbex 7 helper "Make helper text persistent"
      helperV  <- cbex 8 helper "Use helper text as validation message"

      return (dark, rtl, tfConfig <$> dense,
              tfAttrs "my-textfield" <$> disabled <*> required,
              MdHelpTextConfig <$> helper <*> helperP <*> helperV)

  display1_ "Password field with validation"
  el "div" $ do
    let pwConfig = def & attributes .~ constDyn (
          "type" =: "password" <>
          "required" =: "" <>
          "pattern" =: ".{8,}" )
    ti <- mdTextfield def pwConfig "Choose password"
    let err = ffor (_textInput_value ti) $ \v ->
          if T.length v < 8 then "Must be at least 8 characters long" else ""
    mdTextfieldHelpText pwConfig (constDyn (def & helpTextValidationMsg)) $ dynText err

  display1_ "Multi-line Textfields"
  rec
    divTheme dark rtl $ do
      let cfg = textAreaConfig Nothing (Just 8) (Just 40) & over attributes (<> attrs)
      mdTextfieldMulti def cfg "Multi-line label"

    (dark, rtl, attrs) <- el "div" $ do
      disabled <- cbex 10 always "Disabled"
      rtl      <- cbex 11 always "RTL"
      dark     <- cbex 12 always "Dark theme"
      required <- cbex 13 always "Required"
      return (dark, rtl, tfAttrs "multi-line" <$> disabled <*> required)

  display1_ "Full-Width Textfields"
  rec
    divTheme dark (constDyn False) $ do
      let cfg1 = def & attributes .~ attrs & placeholder "Subject"
      mdTextfield tf cfg1 ""
      let cfg2 = textAreaConfig (Just "Message") (Just 8) (Just 40) & over attributes (<> attrs)
      mdTextfieldMulti tf cfg2 ""
      return ()

    (dark, tf, attrs) <- el "div" $ do
      disabledCb <- cbex 20 always "Disabled"
      denseCb    <- cbex 21 always "Dense"
      darkCb     <- cbex 22 always "Dark theme"
      return (darkCb
             , ffor denseCb $ \d -> if d then def & fullwidth & dense else def & fullwidth
             , tfAttrs "full-width" <$> disabledCb <*> pure False)

  return ()

divTheme :: MonadWidget t m => Dynamic t Bool -> Dynamic t Bool -> m a -> m a
divTheme dark rtl = elDynAttr "div" (attrs <$> dark <*> rtl)
  where
    attrs d r = cls d <> dir r
    cls d = if d then "class" =: "mdc-theme--dark" else mempty
    dir r = if r then "dir" =: "rtl" else mempty

cbex :: MonadWidget t m => Int -> Dynamic t Bool -> Text -> m (Dynamic t Bool)
cbex i e t = _checkbox_value <$> el "div" field
  where
    field = mdCheckboxField False (def & attributes .~ (attrs <$> e)) (text t)
    attrs e = "id" =: ("cb" <> tshow i) <> if e then mempty else "disabled" =: ""

tfConfig :: Bool -> MdTextfield
tfConfig d = def & (if d then dense else id)

tfAttrs :: Text -> Bool -> Bool -> Map Text Text
tfAttrs i d r = "id" =: i <> boolAttr "disabled" d <> boolAttr "required" r

boolAttr :: Text -> Bool -> Map Text Text
boolAttr n True = n =: n
boolAttr _ False = mempty
