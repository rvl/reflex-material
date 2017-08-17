{-# LANGUAGE OverloadedStrings #-}

module Reflex.Material.Basic
  ( Style(..)
  , defaultStyle
  , mobile_
  , stylesheet_
  , script_
  , mdcScript
  , scriptDo_
  , style_
  , styles_
  , img_
  , main_
  ) where

import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Reflex.Dom

import Reflex.Material.Types
import Reflex.Material.Typography

data Style
  = Style
  { styleIcons :: [Text]
  , styleFonts :: [Text]
  , styleCss   :: [Text]
  }

defaultStyle :: Style
defaultStyle =
  Style [ "css/icons.css" ]
        [ "css/fonts.css" ]
        [ "css/material-components-web.min.css" ]


mobile_ :: DomBuilder t m => m ()
mobile_ = elAttr "meta" m $ pure ()
  where
    m = "name"    =: "viewport"
     <> "content" =: "width=device-width, initial-scale=1.0"

stylesheet_ :: DomBuilder t m => Text -> m ()
stylesheet_ l = elAttr "link" ss $ pure ()
  where
    ss = "rel"  =: "stylesheet"
      <> "type" =: "text/css"
      <> "href" =: l

script_ :: DomBuilder t m => Text -> m ()
script_ src = elAttr "script" s blank
  where
    s = "type" =: "text/javascript"
      <> "src" =: src

scriptDo_ :: DomBuilder t m => Text -> m ()
scriptDo_ = el "script" . text

style_ :: DomBuilder t m => Text -> m ()
style_ = el "style" . text

styles_ :: DomBuilder t m => Style -> m ()
styles_ ss = do
  mapM_ stylesheet_ (concat [ styleIcons ss, styleFonts ss, styleCss ss ])
  mdcScript

img_ :: DomBuilder t m => Img -> CssClass -> m ()
img_ (Img f w h a) (CssClass c) =
  elAttr "img" (  "class"  =: c
               <> "src"    =: f
               <> "width"  =: (pack . show $ w)
               <> "height" =: (pack . show $ h)
               <> "alt"    =: a
               ) $ pure ()

main_ :: MonadWidget t m => CssClass -> m a -> m ()
main_ t child =
  elClass "main" (unCssClass $ mdcTypography_ <> t) $ do
    _ <- child
    mdcScript

mdcScript :: DomBuilder t m => m ()
mdcScript = script_ "js/material-components-web.min.js"
