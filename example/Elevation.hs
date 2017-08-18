{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Elevation (elevationEx) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Monoid ((<>))
import Reflex.Dom

import Reflex.Material.Common (tshow)
import Reflex.Material.Card (mdcElevation)

elevationEx :: MonadWidget t m => m ()
elevationEx = divClass "elevation-ex" $ do
  elClass "section" "hero" $ do
    demoSurface 0 $ text "FLAT 0dp"
    demoSurface 4 $ text "RAISED 4dp"

  elClass "section" "demo-surfaces" $ do
    forM_ [0..24] demoSurfaceText

  elAttr "section" ("id" =: "hover-el") $ mdo
    let cls n = "mdc-elevation-transition " <> mdcElevation n
        hoverEv = leftmost [0 <$ domEvent Mouseenter e, 2 <$ domEvent Mouseleave e]
    elevDyn <- holdDyn 2 hoverEv
    (e, _) <- elDynClass' "div" (cls <$> elevDyn) $ el "p" $
      text "Hover over or tap me for a transition"
    return ()

demoSurface :: MonadWidget t m => Int -> m () -> m ()
demoSurface n c = elClass "figure" cls $ el "figcaption" c
  where cls = "demo-surface " <> mdcElevation n

demoSurfaceText :: MonadWidget t m => Int -> m ()
demoSurfaceText n = demoSurface n cap
  where cap = do
          text $ tshow n <> "dp ("
          el "code" $ text (mdcElevation n)
          text ")"
