{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Data.Text (Text)
import Reflex.Dom.Core
import Control.Monad (forM)
import Data.Maybe (fromMaybe, isNothing)
import Language.Javascript.JSaddle (JSM)
import Util.DevServer

import Reflex.Material.Basic

import Examples

main :: IO ()
main = devMain 3031

examplesWidget :: JSM ()
examplesWidget = mainWidgetWithHead head_ body_

devMain :: Int -> IO ()
devMain = devServerMain staticServer2 examplesWidget

devMainAutoReload :: Int -> IO ()
devMainAutoReload = devServerMainAutoReload staticServer2 examplesWidget
