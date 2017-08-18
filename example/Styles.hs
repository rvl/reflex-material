{-# LANGUAGE OverloadedStrings #-}

module Styles (exampleCss) where

import Prelude hiding (rem)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)

import Clay
import qualified Clay.Flexbox

exampleCss :: Text
exampleCss = toStrict . render $ do
  common
  elevationEx

common :: Css
common = mempty

elevationEx :: Css
elevationEx = ".elevation-ex" ? do
  ".demo-surfaces" ? do
    paddingTop (px 48)
    display flex
    flexWrap Clay.Flexbox.wrap
    flexDirection row

  ".demo-surface" ? do
    display flex
    alignItems center
    justifyContent center

    width (px 200)
    height (px 100)
    margin (px 0) (px 60) (px 80) (px 0)

    let r = px 3 in borderRadius r r r r
    fontSize (em 0.8)
    color "#9E9E9E"
    background white

  "#hover-el" ? do
    display flex
    alignItems center
    justifyContent center
    let p = rem 2 in padding p p p p
    let r = px 4 in borderRadius r r r r

  ".hero" ? ".demo-surface" ? do
    width (px 96)
    height (px 48)
    backgroundColor "#212121"
    color "#f0f0f0"
