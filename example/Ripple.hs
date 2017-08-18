module Ripple (rippleEx) where

import Data.Monoid ((<>))

import Reflex.Dom

exampleSection :: MonadWidget t m => m () -> (Bool -> m ()) -> m ()
exampleSection title content = elClass "section" "example" $ do
  el "div" $ do
    el "h2" title
    content False
  el "div" $ do
    el "h2" (title >> text " - CSS Only")
    content True

rippleEx :: MonadWidget t m => m ()
rippleEx = divClass "ripple-ex" $ do
  let nojs True = "data-demo-no-js" =: ""
      nojs False = mempty

  elClass "section" "hero mdc-ripple-surface" blank
  let ex1 t = elAttr "div" ("class" =: "mdc-ripple-surface demo-surface mdc-elevation--z2" <> "tabindex" =: "0" <> nojs t) $ text "Interact with me!"
  exampleSection (text "Bounded") ex1

  let ex2 t = elAttr "div" ("class" =: "mdc-ripple-surface demo-surface material-icons" <> "data-mdc-ripple-is-unbounded" =: ""  <> "aria-label" =: "Favorite" <> "tabindex" =: "0" <> nojs t) $ text "favorite"
  exampleSection (text "Unbounded") ex2

  let ex3 t = do
        elAttr "div" ("class" =: "mdc-ripple-surface mdc-ripple-surface--primary mdc-theme--primary demo-surface mdc-elevation--z2" <> "tabindex" =: "0" <> nojs t) (text "Primary")
        elAttr "div" ("class" =: "mdc-ripple-surface mdc-ripple-surface--accent mdc-theme--accent demo-surface mdc-elevation--z2" <> "tabindex" =: "0" <> nojs t) (text "Accent")

  exampleSection (text "Theme Styles") ex3

  let ex4 t = elAttr "button" ("type" =: "button" <> "class" =: "mdc-ripple-surface mdc-elevation--z2 demo-surface" <> nojs t) (text "button")
  exampleSection (text "Applied to " >> el "code" (text "button") >> text " element") ex4

  el "p" $ text "Note that this example must be compiled with ghcjs for ripples and other javascript effects to work."
