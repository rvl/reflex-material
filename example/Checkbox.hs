module Checkbox where

import Data.Monoid ((<>), mempty)
import Reflex.Dom
import Data.Map (Map)
import Data.Text (Text)

import Reflex.Material.Button
import Reflex.Material.Checkbox
import Reflex.Material.Typography
import Reflex.Material.Common

checkboxEx :: MonadWidget t m => m ()
checkboxEx = do
  title_ "MDC Checkbox"
  el "div" $ do
    display1_ "With Javascript"
    rec
      dynInd <- toggle False clickInd
      dynDis <- toggle False clickDis
      let cfg = def & attributes .~ (cbAttrs "cb1" <$> dynInd <*> dynDis)
      mdCheckboxField False cfg $ text "This is my checkbox"
      clickInd <- mdButton (raised $ dense $ def) $ text "Make indeterminate"
      clickDis <- mdButton (def & raised & dense) $ text "Toggle disabled"
    return ()

  divClass "mdc-theme--dark" $ do
    display1_ "Dark theme"
    rec
      disabled <- toggle False click
      let cfg = def & attributes .~ (cbAttrs "cb2" False <$> disabled)
      mdCheckboxField False cfg $ text "This is my checkbox"
      click <- mdButton (pure def) $ text "Toggle disabled"
    return ()

-- fixme: indeterminate attribute setting doesn't work in HTML
-- it needs to be done with javascript.
cbAttrs :: Text -> Bool -> Bool -> Map Text Text
cbAttrs id ind dis = "id" =: id <> attr "indeterminate" ind <> attr "disabled" dis
  where
    attr name True = name =: name
    attr _ False = mempty
