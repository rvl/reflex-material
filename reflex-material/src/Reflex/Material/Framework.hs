{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Reflex.Material.Framework
  ( attachRipple
  , attachTextField
  , attachCheckbox
  , attachIconToggle
  , attachFormField
  , attachSelect
  , attachList
  , attachMenu
  , attachMenuSurface
  , attachFloatingLabel
  , attachLineRipple
  , attachTopAppBar
  , mdcAttach
  , mdcAttachInit
  , ComponentRef
  ) where

import Prelude hiding ((!!))

import Control.Lens ((^.))
import Data.Text (Text)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)

import qualified Data.JSString as JSString
import Language.Javascript.JSaddle (JSVal, js, js0, js1, js2, js3, jss, jsg, fun, function, JSCallAsFunction, isTruthy, jsUndefined, ghcjsPure, strictEqual, setProp, toJSBool, valToObject, Object, toJSVal, getProp, fromJSValUnchecked, (!), (!!), MakeArgs, jsf, jsNull)
import qualified JSDOM.Types as DOM
import JSDOM.Types (JSM, MonadJSM(..), liftJSM)
import JSDOM.Element as DOM (getElementsByTagName)

import Reflex.Dom.Core

import Reflex.Material.Common (MaterialWidget)

----------------------------------------------------------------------------
-- Notes on framework integration are here:
--   https://github.com/material-components/material-components-web/blob/master/docs/integrating-into-frameworks.md
--
-- This module uses the simple method of integration. Writing an
-- adapter doesn't seem to be necessary.
----------------------------------------------------------------------------

----------------------------------------------------------------------------
newtype ComponentRef = ComponentRef { unComponentRef :: JSVal }
newtype MdcRef = MdcRef { unMdcRef :: JSVal }
----------------------------------------------------------------------------

attachRipple :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachRipple = mdcAttach "ripple" "MDCRipple"

attachLineRipple :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachLineRipple = mdcAttach "lineRipple" "MDCLineRipple"

attachFloatingLabel :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachFloatingLabel = mdcAttach "floatingLabel" "MDCFloatingLabel"

attachTextField :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachTextField = mdcAttach "textField" "MDCTextField"

attachIconToggle :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachIconToggle = mdcAttach "iconToggle" "MDCIconToggle"

attachFormField
  :: MaterialWidget t m
  => Element EventResult (DomBuilderSpace m) t -- ^ The form field element
  -> Maybe (Element EventResult (DomBuilderSpace m) t) -- ^ The embedded input element
  -> m ()
attachFormField formField input =
  mdcAttachInit "formField" "MDCFormField" formField $ \_ component ->
    maybe (pure ()) (setFormFieldInput component . _element_raw) input

attachList :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachList = mdcAttach "list" "MDCList"

attachMenuSurface :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachMenuSurface = mdcAttach "menuSurface" "MDCMenuSurface"

attachTopAppBar :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachTopAppBar = mdcAttach "topAppBar" "MDCTopAppBar"

----------------------------------------------------------------------------

mdcAttach
  :: (MaterialWidget t m)
  => Text -> Text
  -> Element EventResult (DomBuilderSpace m) t
  -> m ()
mdcAttach mod com = flip registerAttach' (js_attachTo mod com) . _element_raw

mdcAttachInit
  :: (MaterialWidget t m)
  => Text -> Text
  -> Element EventResult (DomBuilderSpace m) t
  -> (DOM.Element -> ComponentRef -> DOM.JSM ())
  -> m ()
mdcAttachInit mod com elm setup =
  registerAttach' (_element_raw elm) $ \dom mdc -> do
    component <- js_attachTo mod com dom mdc
    setup dom component

registerAttach'
  :: (MaterialWidget t m, MonadJSM (Performable m), PostBuild t m)
  => DOM.Element
  -> (DOM.Element -> MdcRef -> JSM a)
  -> m ()
registerAttach' elm attachJS = do
  mdc <- getMdcLoad
  performEvent_ (liftJSM . void . attachJS elm <$> mdc)
  pure ()
  -- fixme: how to clean up?
  -- pd <- getPreDestroy
  -- performEvent_ $ (liftJSM . js_mdcDetach $ el) <$ pd

-- | Event which occurs after postBuild and once mdc javascript is loaded.
getMdcLoad :: MaterialWidget t m => m (Event t MdcRef)
getMdcLoad = do
  -- Delay after postBuild is to ensure that dom elements exist.
  pb <- delay 0 =<< getPostBuild
  (eLoadMdc, onLoadMdc) <- newTriggerEvent

  let setup = js_onLoadMdc $ fun $ \_ _ [mdc] ->
        liftIO $ onLoadMdc (MdcRef mdc)

  performEvent_ (liftJSM setup <$ pb)
  pure eLoadMdc

getMdc :: JSM (Maybe JSVal)
getMdc = do
  window <- jsg ("window" :: Text)
  mdc <- window ^. js ("mdc" :: Text)
  haveMdc <- ghcjsPure $ isTruthy mdc
  pure (if haveMdc then Just mdc else Nothing)

-- | Run a callback once the mdc script has loaded. If it iss already
-- loaded (indicated by the @window.mdc@ property), run the callback
-- immediately.
js_onLoadMdc :: JSCallAsFunction -> JSM ()
js_onLoadMdc cb = getMdc >>= \case
  Just mdc -> cb jsUndefined jsUndefined [mdc]
  Nothing -> do
    document <- jsg ("document" :: Text)
    let listener = function $ \_fun _this [evt] -> do
          tagName <- evt ^. js ("target" :: Text) . js ("tagName" :: Text)
          isScriptLoaded <- strictEqual tagName (JSString.pack "SCRIPT")
          when isScriptLoaded $ getMdc >>= \case
            Just mdc -> do
              cb jsUndefined jsUndefined [mdc]
              void $ document ^. js3 ("removeEventListener" :: Text) ("load" :: Text) listener True
            Nothing -> pure ()

    void $ document ^. js3 ("addEventListener" :: Text) ("load" :: Text) listener True

js_attachTo :: Text -> Text -> DOM.Element -> MdcRef -> DOM.JSM ComponentRef
js_attachTo mod com e (MdcRef mdc) = do
  r <- mdc ^. js mod . js com . js1 ("attachTo" :: Text) e
  e ^. jss ("mdcComponent" :: Text) r
  pure $ ComponentRef r

js_mdcDetach :: DOM.Element -> DOM.JSM ()
js_mdcDetach e = void (e ^. js ("mdcComponent" :: Text) . js0 ("destroy" :: Text))

-- Extract the component ref from a DOM element. The ref is set by
-- 'js_attachTo'.
getComponent :: DOM.Element -> JSM (Maybe Object)
getComponent elm = do
  component <- elm ^. js ("mdcComponent" :: Text)
  isComponent <- ghcjsPure $ isTruthy component
  if isComponent
    then Just <$> valToObject component
    else pure Nothing

----------------------------------------------------------------------------

consoleLog :: (MakeArgs args) => args -> JSM ()
consoleLog args = do
  w <- jsg ("console" :: Text)
  w ^. jsf ("log" :: Text) args
  pure ()

----------------------------------------------------------------------------

attachCheckbox :: MaterialWidget t m
               => Maybe (Event t Bool)
               -> Element EventResult (DomBuilderSpace m) t
               -> m ()
attachCheckbox eIndeterminate elm = do
  mdcAttach "checkbox" "MDCCheckbox" elm

  case eIndeterminate of
    Just set ->
      performEvent_ (liftJSM . setIndeterminate (_element_raw elm) <$> set)
    Nothing -> pure ()

setIndeterminate :: DOM.Element -> Bool -> JSM ()
setIndeterminate elm i = getComponent elm >>= \case
  Just c -> setProp "indeterminate" (toJSBool i) c
  Nothing -> pure ()

----------------------------------------------------------------------------

attachSelect :: forall t m. MaterialWidget t m
             => Maybe (Event t Int)
             -> Element EventResult (DomBuilderSpace m) t
             -> m (Event t Int)
attachSelect mSetValue elm = do
  let elm' = _element_raw elm

  case mSetValue of
    Just set ->
      performEvent_ $ liftJSM . setValueMdSelect elm' <$> set
    Nothing -> return ()

  (eChange, onChange) <- newTriggerEvent

  mdcAttachInit "select" "MDCSelect" elm $ \elm' component -> do
    js_setupSelectChangeListener elm' $ fun $ \_ _ _args -> do
      v <- getValueMdSelect component
      liftIO $ onChange v

  pure eChange

setValueMdSelect :: DOM.Element -> Int -> JSM ()
setValueMdSelect elm i = do
  getComponent elm >>= \case
    Just comp -> do
      i' <- toJSVal i
      setProp "selectedIndex" i' comp
    Nothing -> pure ()

getValueMdSelect :: ComponentRef -> JSM Int
getValueMdSelect (ComponentRef val) =
  valToObject val >>= getProp "selectedIndex" >>= fromJSValUnchecked

js_setupSelectChangeListener :: DOM.Element -> JSCallAsFunction -> DOM.JSM ()
js_setupSelectChangeListener elm cb = do
  let listener = fun $ \_ this _ -> do
        elm' <- valToObject elm
        comp <- getProp "component" elm'
        cb jsUndefined this [comp]

  void $ elm ^. js2 ("addEventListener" :: Text) ("MDCSelect:change" :: Text) listener
  pure () -- fixme: return a function to clean up listener

----------------------------------------------------------------------------

attachMenu :: MaterialWidget t m => Event t Bool -> Element EventResult (DomBuilderSpace m) t -> m (Event t Int)
attachMenu eShow elm = do
  (eSelected, onSelected) <- newTriggerEvent
  (eCancel, onCancel) <- newTriggerEvent

  mdcAttachInit "menu" "MDCMenu" elm $ \elm' _ -> do
    js_setupMenuCancelListener elm' (fun $ \_ _ _ -> liftIO $ onCancel ())

    let selected = fun $ \_ _ [index] -> do
              index' <- fromJSValUnchecked index
              liftIO $ onSelected index'
    js_setupMenuSelectedListener elm' selected

  -- fixme: might crash if dom isn't created yet
  performEvent_ $ liftJSM . setMenuOpen (_element_raw elm) . const True <$> eShow

  pure eSelected

setMenuOpen :: DOM.Element -> Bool -> JSM ()
setMenuOpen el i = getComponent el >>= \case
  Just comp -> setProp "open" (toJSBool i) comp
  Nothing -> pure()

js_setupMenuSelectedListener :: DOM.Element -> JSCallAsFunction -> DOM.JSM ()
js_setupMenuSelectedListener elm cb = do
  let listener = fun $ \_ this [evt] -> do
        detail <- getProp "detail" =<< valToObject evt
        index <- detail ! ("index" :: Text)
        cb jsUndefined this [index]
  void $ elm ^. js2 ("addEventListener" :: Text) ("MDCMenu:selected" :: Text) listener

js_setupMenuCancelListener :: DOM.Element -> JSCallAsFunction -> DOM.JSM ()
js_setupMenuCancelListener elm cb =
  addEventListener ("MDCMenu:cancel" :: Text) listener
  where
    listener = fun $ \_ this [] -> cb jsUndefined this []
    addEventListener name l = void $ elm ^. js2 ("addEventListener" :: Text) name l

----------------------------------------------------------------------------

-- | Connects a MDC Form Field with its inner component element.
setFormFieldInput :: ComponentRef -> DOM.Element -> JSM ()
setFormFieldInput ffComponent cb = do
  cbComponent <- cb ^. js ("mdcComponent" :: Text)
  void $ unComponentRef ffComponent ^. jss ("input" :: Text) cbComponent
