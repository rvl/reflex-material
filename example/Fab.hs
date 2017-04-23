module Fab where

import Data.Monoid ((<>), mempty)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Map as M

import Reflex.Dom

import Reflex.Material.Fab
import Reflex.Material.Common
import Reflex.Material.Typography

fab :: MonadWidget t m => m ()
fab = do
  title_ "MDC FAB"

  mdFab (constDyn (def & attributes .~ ("id" =: "demo-absolute-fab"))) pencilSvg

  el "fieldset" $ do
    el "legend" $ text "Normal FABs"
    fabs

  elAttr "fieldset" ("disabled" =: "") $ do
    el "legend" $ text "Disabled FABs"
    fabs

fabs :: MonadWidget t m => m ()
fabs = do
  mdFabIcon def "favorite_border" "Favorite"
  mdFabIcon (def & mini) "favorite_border" "Favorite"
  mdFabIcon (def & plain) "favorite_border" "Favorite"
  mdFabIcon (def & mini & plain) "favorite_border" "Favorite"
  return ()

pencilSvg :: MonadWidget t m => m ()
pencilSvg = do
  element "svg" (def & namespace .~ ns & initialAttributes .~ (attrs [("width", "24"), ("height", "24"), ("viewBox", "0 0 24 24")])) $
    element "path" (def & namespace .~ ns & initialAttributes .~ (attrs [("d", "M3 17.25V21h3.75L17.81 9.94l-3.75-3.75L3 17.25zM20.71 7.04c.39-.39.39-1.02 0-1.41l-2.34-2.34c-.39-.39-1.02-.39-1.41 0l-1.83 1.83 3.75 3.75 1.83-1.83z")])) blank
  return ()
  where
    ns = Just "http://www.w3.org/2000/svg"
    attrs ps = M.fromList [(AttributeName ns attr, val) | (attr, val) <- ps]
