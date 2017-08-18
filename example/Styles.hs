{-# LANGUAGE OverloadedStrings #-}

module Styles (exampleCss) where

import Prelude hiding (rem, div, (**))
import Data.Text (Text)
import Data.Text.Lazy (toStrict)

import Clay
import qualified Clay.Flexbox
import qualified Clay.Media

exampleCss :: Text
exampleCss = toStrict . render $ do
  elevationEx
  rippleEx

elevationEx :: Css
elevationEx = ".elevation-ex" ? do
  ".demo-surfaces" ? do
    paddingTop (px 48)
    display flex
    flexWrap Clay.Flexbox.wrap
    flexDirection row

  demoSurface $ do
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

demoSurface :: Css -> Css
demoSurface extra = ".demo-surface" ? do
    display flex
    alignItems center
    justifyContent center
    width (px 200)
    height (px 100)
    extra

rippleEx :: Css
rippleEx = ".ripple-ex" ? do
  demoSurface $ do
    let p = rem 1 in padding p p p p
    cursor pointer
    userSelect none

  ".mdc-ripple-surface[data-mdc-ripple-is-unbounded]" ? do
    width (px 40)
    height (px 40)
    let z = px 0 in padding z z z z
    let r = pc 50 in borderRadius r r r r

  ".mdc-ripple-surface[data-mdc-ripple-is-unbounded][data-demo-no-js]::before" **
    ".mdc-ripple-surface[data-mdc-ripple-is-unbounded][data-demo-no-js]::after" ? do
    height (pc 100)
    width (pc 100)
    left (px 0)
    top (px 0)

  "button.demo-surface" ? do
    -- display inlineBblock -- for Safari apparently
    backgroundColor none
    borderStyle4 none none none none
    "-webkit-appearance" -: "none"
    "-moz-appearance" -: "none"
    "appearance" -: "none"

  query Clay.Media.screen [Clay.Media.maxWidth (px 565)] $ ".example" ? do
    flexDirection column
    width (pc 100)

  ".example" ? do
    let m = px 24 in margin m m m m
    let p = px 24 in padding p p p p
    alignItems center
    display flex
    marginLeft (px 10)

  ".example" |> div ? do
    margin (px 48) 0 (px 48) 0
    width (px 400)

  ".example" ? h2 ? do
    fontSize (em 1.2)
    marginBottom (em 0.8)
