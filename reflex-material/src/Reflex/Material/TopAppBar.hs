{-# LANGUAGE OverloadedStrings #-}

module Reflex.Material.TopAppBar
  ( Fixity(..)
  , TopAppBarAlign(..)
  , mdcTopAppBar_
  , mdcTopAppBarFixed_
  , mdcTopAppBarFixedAdjust_
  , mdcTopAppBarRow_
  , mdcTopAppBarSection_
  , mdcTopAppBarSectionAlignStart_
  , mdcTopAppBarSectionAlignEnd_
  , mdcTopAppBarTitle_
  , topAppBarHeader_
  , topAppBarRow_
  , topAppBarSection_
  , topAppBarTitle_
  , topAppBar_
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Reflex.Dom

import Reflex.Material.Types

data Fixity = Fixed | NotFixed

data TopAppBarAlign = AlignStart | AlignCenter | AlignEnd

mdcTopAppBar_ :: CssClass
mdcTopAppBar_ = CssClass "mdc-top-app-bar"

mdcTopAppBarFixed_ :: CssClass
mdcTopAppBarFixed_ = CssClass "mdc-top-app-bar--fixed"

mdcTopAppBarFixedAdjust_ :: CssClass
mdcTopAppBarFixedAdjust_ = CssClass "mdc-top-app-bar-fixed-adjust"

mdcTopAppBarRow_ :: CssClass
mdcTopAppBarRow_ = CssClass "mdc-top-app-bar__row"

mdcTopAppBarSection_ :: CssClass
mdcTopAppBarSection_ = CssClass "mdc-top-app-bar__section"

mdcTopAppBarSectionAlignStart_ :: CssClass
mdcTopAppBarSectionAlignStart_ = CssClass "mdc-top-app-bar__section--align-start"

mdcTopAppBarSectionAlignEnd_ :: CssClass
mdcTopAppBarSectionAlignEnd_ = CssClass "mdc-top-app-bar__section--align-end"

mdcTopAppBarTitle_ :: CssClass
mdcTopAppBarTitle_ = CssClass "mdc-top-app-bar__title"

topAppBarHeader_ :: DomBuilder t m => Fixity -> m a -> m a
topAppBarHeader_ NotFixed = elClass "header" (unCssClass mdcTopAppBar_)
topAppBarHeader_ Fixed    = elClass "header" (unCssClass $ mdcTopAppBar_ <> mdcTopAppBarFixed_)

topAppBarRow_ :: DomBuilder t m => CssClass -> m a -> m a
topAppBarRow_ t = elClass "div" (unCssClass $ mdcTopAppBarRow_ <> t)

topAppBarSection_ :: DomBuilder t m => CssClass -> m a -> m a
topAppBarSection_ t = elClass "section" (unCssClass $ mdcTopAppBarSection_ <> t)

topAppBarTitle_ :: DomBuilder t m => Text -> m ()
topAppBarTitle_ = elClass "span" (unCssClass mdcTopAppBarTitle_) . text

topAppBar_ :: DomBuilder t m => Fixity -> TopAppBarAlign -> Maybe Text -> m a -> m a
topAppBar_ f x t c =
  topAppBarHeader_ f $
    topAppBarRow_ mempty $
      topAppBarSection_ tba $
        case t of
          Nothing -> c
          Just t' -> topAppBarTitle_ t' >> c
  where
    tba = case x of
            AlignStart  -> mdcTopAppBarSectionAlignStart_
            AlignCenter -> CssClass ""
            AlignEnd    -> mdcTopAppBarSectionAlignEnd_
