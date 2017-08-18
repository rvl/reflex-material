{-# LANGUAGE OverloadedStrings, CPP, TemplateHaskell #-}

module Main where

import Data.Monoid ((<>))
import Data.Text (Text)
import Reflex.Dom.Core
import Control.Monad (forM)
import Data.Maybe (fromMaybe, isNothing)
import Language.Javascript.JSaddle (JSM)

import Reflex.Material.Basic

import Examples

#ifdef GHCJS_BROWSER
main :: IO ()
main = examplesWidget

#else
import Util.DevServer

main :: IO ()
main = devMain 3031

devMain :: Int -> IO ()
devMain = devServerMain staticServer2 examplesWidget

devMainAutoReload :: Int -> IO ()
devMainAutoReload = devServerMainAutoReload staticServer2 examplesWidget
#endif

examplesWidget :: JSM ()
examplesWidget = mainWidgetWithHead head_ body_
