{-# LANGUAGE OverloadedStrings #-}

module Reflex.Material.Types
  ( Img(..)
  , CssClass(..)
  , classAttr
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Map (Map, singleton)

newtype CssClass = CssClass { unCssClass :: Text }

instance Semigroup CssClass where
  (CssClass a) <> (CssClass b) = CssClass (a <> " " <> b)

instance Monoid CssClass where
  mempty = CssClass ""

classAttr :: CssClass -> Map Text Text
classAttr c = singleton "class" (unCssClass c)

data Img = Img Text Int Int Text -- file, width, height, alt
