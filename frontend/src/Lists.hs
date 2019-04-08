{-# LANGUAGE OverloadedStrings #-}

module Lists (listEx) where

import Control.Monad (replicateM_)
import Data.Monoid ((<>))
import Reflex.Dom

import Reflex.Material.Basic
import Reflex.Material.Icon
import Reflex.Material.List
import Reflex.Material.Types
import Reflex.Material.Typography

listEx :: DomBuilder t m => m ()
listEx = do
  display3_ "List"
  title_ "Default"
  el "div" $ list_ "ul" mempty $
    replicateM_ 3 $ item_ "li" mempty $ text "Line of text"
  title_ "Dense"
  el "div" $ list_ "ul" mempty $
    replicateM_ 3 $ item_ "li" mdcListDense_ $ text "Line of text"

  display3_ "List Start Detail"
  title_ "Default Start Detail"
  el "div" $ list_ "ul" mempty $
    replicateM_ 3 $ item_ "li" mempty $ do
      elClass "span" (unCssClass $ CssClass "grey-bg" <> mdcListItemGraphic_) $ pure ()
      text "Line of text"
  title_ "Start Detail Dense"
  el "div" $ list_ "ul" mdcListDense_ $
    replicateM_ 3 $ item_ "li" mempty $ do
      elClass "span" (unCssClass $ CssClass "grey-bg" <> mdcListItemGraphic_) $ pure ()
      text "Line of text"
  title_ "Start Detail Icon with Text"
  el "div" $ list_ "ul" mempty $ do
    item_ "li" mempty $ do
      mdIconClass "network_wifi" mdcListItemGraphic_
      text "Network Wifi"
    item_ "li" mempty $ do
      mdIconClass "bluetooth" mdcListItemGraphic_
      text "Bluetooth"
    item_ "li" mempty $ do
      mdIconClass "data_usage" mdcListItemGraphic_
      text "Data Usage"

  title_ "List Avatar"
  el "div" $ list_ "ul" mdcListAvatarList_ $ do
    item_ "li" mempty $ do
      img_ (Img "static/imagesBrian.png" 56 56 "Brian") mdcListItemGraphic_
      text "Brian"
    item_ "li" mempty $ do
      img_ (Img "static/imagesChris.png" 56 56 "Chris") mdcListItemGraphic_
      text "Chris"

  display3_ "List End Detail"
  title_ "Default End Detail"
  el "div" $ list_ "ul" mempty $
    replicateM_ 3 $ item_ "li" mempty $ do
      text "Line of text"
      elClass "span" (unCssClass $ CssClass "grey-bg" <> mdcListItemMeta_) $ pure ()
  title_ "End Detail Dense"
  el "div" $ list_ "ul" mdcListDense_ $
    replicateM_ 3 $ item_ "li" mempty $ do
      text "Line of text"
      elClass "span" (unCssClass $ CssClass "grey-bg" <> mdcListItemMeta_) $ pure ()
  title_ "End Detail Avatar"
  el "div" $ list_ "ul" mdcListAvatarList_ $ do
    item_ "li" mempty $ do
      img_ (Img "static/imagesBrian.png" 56 56 "Brian") mdcListItemGraphic_
      text "Brian"
      elClass "span" (unCssClass $ CssClass "grey-bg" <> mdcListItemMeta_) $ pure ()
    item_ "li" mempty $ do
      img_ (Img "static/imagesChris.png" 56 56 "Chris") mdcListItemGraphic_
      text "Chris"
      elClass "span" (unCssClass $ CssClass "grey-bg" <> mdcListItemMeta_) $ pure ()

  display3_ "Two Line List"
  title_ "Default Two Line"
  el "div" $ list_ "ul" mdcListTwoLine_ $
    replicateM_ 3 $ itemTwo_ "li" mempty (text "Line of text") (text "Line of text")
  title_ "Two Line Dense"
  el "div" $ list_ "ul" (mdcListTwoLine_ <> mdcListDense_) $
    replicateM_ 3 $ itemTwo_ "li" mempty (text "Line of text") (text "Line of text")

  display3_ "Two Line Start Detail"
  title_ "Two Line Start Detail"
  el "div" $ list_ "ul" mdcListTwoLine_ $
    replicateM_ 3 $ item_ "span" mempty $ do
      elClass "span" (unCssClass $ CssClass "grey-bg" <> mdcListItemGraphic_) $ pure ()
      itemTwo_ "span" mempty (text "Line of text") (text "Line of text")
  title_ "Two Line Start Detail Dense"
  el "div" $ list_ "ul" (mdcListTwoLine_ <> mdcListDense_) $
    replicateM_ 3 $ item_ "span" mempty $ do
      elClass "span" (unCssClass $ CssClass "grey-bg" <> mdcListItemGraphic_) $ pure ()
      itemTwo_ "span" mempty (text "Line of text") (text "Line of text")

  display3_ "Two Line End Detail"
  title_ "Two Line End Detail"
  el "div" $ list_ "ul" mdcListTwoLine_ $
    replicateM_ 3 $ item_ "li" mempty $ do
      itemTwo_ "span" mempty (text "Line of text") (text "Line of text")
      elClass "span" (unCssClass $ CssClass "grey-bg" <> mdcListItemMeta_) $ pure ()
  title_ "Two Line Start Detail Dense"
  el "div" $ list_ "ul" (mdcListTwoLine_ <> mdcListDense_) $
    replicateM_ 3 $ item_ "li" mempty $ do
      itemTwo_ "span" mempty (text "Line of text") (text "Line of text")
      elClass "span" (unCssClass $ CssClass "grey-bg" <> mdcListItemMeta_) $ pure ()
