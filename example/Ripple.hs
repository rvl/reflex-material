module Ripple (rippleEx) where

import Data.Monoid ((<>))
import Control.Monad (void, mapM_)

import Reflex.Dom

import Reflex.Material.Framework (attachRipple)

exampleSection :: MonadWidget t m => m () -> m ([El t]) -> m ()
exampleSection title content = elClass "section" "example" $ do
  el "div" $ do
    el "h2" title
    content >>= mapM_ attachRipple
  el "div" $ do
    el "h2" (title >> text " - CSS Only")
    void content

rippleEx :: MonadWidget t m => m ()
rippleEx = divClass "ripple-ex" $ do
  (h, _) <- elClass' "section" "hero mdc-ripple-surface" blank
  attachRipple h

  let els = fmap (\(e, _) -> [e])

  let ex1 = els (elAttr' "div" ("class" =: "mdc-ripple-surface demo-surface mdc-elevation--z2" <> "tabindex" =: "0") (text "Interact with me!"))
  exampleSection (text "Bounded") ex1

  let ex2 = els (elAttr' "div" ("class" =: "mdc-ripple-surface demo-surface material-icons" <> "data-mdc-ripple-is-unbounded" =: ""  <> "aria-label" =: "Favorite" <> "tabindex" =: "0") (text "favorite"))
  exampleSection (text "Unbounded") ex2

  let ex3 = do
        (e1, _) <- elAttr' "div" ("class" =: "mdc-ripple-surface mdc-ripple-surface--primary mdc-theme--primary demo-surface mdc-elevation--z2" <> "tabindex" =: "0") (text "Primary")
        (e2, _) <- elAttr' "div" ("class" =: "mdc-ripple-surface mdc-ripple-surface--accent mdc-theme--accent demo-surface mdc-elevation--z2" <> "tabindex" =: "0") (text "Accent")
        return [e1, e2]

  exampleSection (text "Theme Styles") ex3

  let ex4 = els (elAttr' "button" ("type" =: "button" <> "class" =: "mdc-ripple-surface mdc-elevation--z2 demo-surface") (text "button"))
  exampleSection (text "Applied to " >> el "code" (text "button") >> text " element") ex4

  el "p" $ text "Note that this example must be compiled with ghcjs for ripples and other javascript effects to work."
