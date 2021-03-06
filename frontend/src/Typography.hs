{-# LANGUAGE OverloadedStrings #-}

module Typography (typographyEx) where

import Reflex.Dom

import Reflex.Material.Typography

typographyEx :: DomBuilder t m => m ()
typographyEx = do
  display3_ "Typography"
  display4_ "Display 4"
  display3_ "Display 3"
  display2_ "Display 2"
  display1_ "Display 1"
  subheading2_ "Sub Heading 2"
  subheading1_ "Sub Heading 1"
  body1_ "Body 1"
  body2_ "Body 2"
