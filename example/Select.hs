module Select where

import Data.Monoid ((<>), mempty)
import Reflex.Dom
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.Fix (MonadFix)

import Reflex.Material.Select
import Reflex.Material.Checkbox
import Reflex.Material.Button
import Reflex.Material.Typography
import Reflex.Material.Common

options :: Map Text Text
options = M.fromList [ ("grains", "Bread, Cereal, Rice, and Pasta")
                     , ("vegetables", "Vegetables")
                     , ("fruit", "Fruit")
                     , ("dairy", "Milk, Yogurt, and Cheese")
                     , ("meat", "Meat, Poultry, Fish, Dry Beans, Eggs, and Nuts")
                     , ("fats", "Fats, Oils, and Sweets")
                     ]

selectEx :: (MaterialWidget t m, MonadFix m, MonadHold t m, PostBuild t m) => m ()
selectEx = do
  title_ "MDC select"
  el "div" $ do
    display1_ "Fully-Featured Component"
    rec
      dd <- elDynAttr "section" (sectionAttrs <$> dynDark <*> dynRtl <*> dynDis) $
        mdSelect "" (constDyn options) (def & setValue .~ leftmost [eVeg, eClear])

      el "div" $ do
        dynText (ffor (value dd) $ \v -> "Currently selected: \"" <> v <> "\"")

      (eVeg, eClear) <- el "div" $ do
        b1 <- mdButton def (text "Eat More Vegetables")
        b2 <- mdButton def (text "Clear")
        return (fmap (const "vegetables") b1, fmap (const "") b2)

      dynDark <- cbex "dark" "Dark"
      dynDis <- cbex "disabled" "Disabled"
      dynRtl <- cbex "rtl" "RTL"
    return dd

  el "div" $ do
    display1_ "Select Multiple - CSS Only"
    el "section" $ do
      mdSelectMultiple 8 $ do
        mdOptGroup "Fats, Oils, & Sweets" $ do
          mdOption "Olive Oil"
          mdOption "Brown Sugar"
          mdOption "Ice Cream"
        mdDivider
        mdOptGroup "Dairy" $ do
          mdOption "Milk"
          mdOption "Cheese"
          mdOption "More Cheese"
    return ()

sectionAttrs :: Bool -> Bool -> Bool -> Map Text Text
sectionAttrs dark rtl disabled = dark' <> rtl' <> disabled' <> "id" =: "demo-wrapper"
  where dark' = if dark then "class" =: "mdc-theme--dark" else mempty
        rtl' = if rtl then "dir" =: "rtl" else mempty
        disabled' = if disabled then "disabled" =: "disabled" else mempty

cbex :: MaterialWidget t m => Text -> Text -> m (Dynamic t Bool)
cbex i t = _checkbox_value <$> el "div" field
  where
    field = mdCheckboxField False (def & attributes .~ attrs) (text t)
    attrs = constDyn ("id" =: ("cb" <> i))
