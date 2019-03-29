{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Reflex.Material.Framework
  ( attachRipple
  , attachTextfield
  , attachCheckbox
  , attachIconToggle
  , attachFormField
  , attachSelect
  , attachSimpleMenu
  , MaterialWidget
  ) where

import qualified GHCJS.DOM.Types as DOM
import GHCJS.DOM.Types (JSM, MonadJSM(..), liftJSM)
import Control.Lens ((^.))
import Data.Text (Text)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)

import Reflex.Dom.Core
import Language.Javascript.JSaddle (JSVal, js, js0, js1, js2, js3, jss, jsg, fun, function, JSCallAsFunction, isTruthy, jsUndefined, ghcjsPure, strictEqual, JSString(..), setProp, toJSBool, valToObject, Object, toJSVal, getProp, fromJSValUnchecked, (!))

import Reflex.Material.Common (MaterialWidget)

----------------------------------------------------------------------------
-- Notes on framework integration are here:
--   https://github.com/material-components/material-components-web/blob/master/docs/architecture.md
-- This module uses the simple method of integration. Writing an
-- adapter doesn't seem to be necessary.
----------------------------------------------------------------------------

----------------------------------------------------------------------------
newtype MdcRef = MdcRef { unMdcRef :: JSVal }
----------------------------------------------------------------------------

attachRipple :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachRipple = registerAttach (mdcAttach "ripple" "MDCRipple")

attachTextfield :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachTextfield = registerAttach (mdcAttach "textfield" "MDCTextfield")

attachIconToggle :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachIconToggle = registerAttach (mdcAttach "iconToggle" "MDCIconToggle")

attachFormField :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachFormField = registerAttach (mdcAttach "formField" "MDCFormField")

----------------------------------------------------------------------------

registerAttach
  :: (MaterialWidget t m)
  => (DOM.Element -> DOM.JSM MdcRef)
  -> Element EventResult (DomBuilderSpace m) t
  -> m ()
registerAttach attachJS = registerAttach' attachJS . _element_raw

registerAttach'
  :: (MaterialWidget t m, MonadJSM (Performable m), PostBuild t m)
  => (DOM.Element -> JSM MdcRef) -> DOM.Element -> m ()
registerAttach' attachJS e = do
  pb <- getMdcLoad
  performEvent_ ((liftJSM . void . attachJS $ e) <$ pb)
  pure ()
  -- fixme: how to clean up?
  -- pd <- getPreDestroy
  -- performEvent_ $ (liftJSM . js_mdcDetach $ el) <$ pd


-- | Event which occurs after postBuild and once mdc javascript is loaded.
getMdcLoad :: MaterialWidget t m => m (Event t ())
getMdcLoad = do
  pb <- getPostBuild
  (eLoadMdc, onLoadMdc) <- newTriggerEvent

  let
    setup = do
      js_onLoadMdc $ fun $ \_ _ _args -> liftIO (onLoadMdc ())
      pure eLoadMdc

  res <- performEvent (liftJSM setup <$ pb)
  switchHold pb res

getMdc :: JSM (Maybe JSVal)
getMdc = do
  window <- jsg ("window" :: Text)
  mdc <- window ^. js ("mdc" :: Text)
  haveMdc <- ghcjsPure $ isTruthy mdc
  pure (if haveMdc then Just mdc else Nothing)

js_onLoadMdc :: JSCallAsFunction -> JSM ()
js_onLoadMdc cb = getMdc >>= \case
  Just mdc -> cb jsUndefined jsUndefined [mdc]
  Nothing -> do
    document <- jsg ("document" :: Text)
    let listener = function $ \_fun _this [evt] -> do
          tagName <- evt ^. js ("target" :: Text) . js ("tagName" :: Text)
          isScriptLoaded <- strictEqual tagName (JSString "SCRIPT")
          when isScriptLoaded $ getMdc >>= \case
            Just mdc -> do
              cb jsUndefined jsUndefined [mdc]
              void $ document ^. js3 ("removeEventListener" :: Text) ("load" :: Text) listener True
            Nothing -> pure ()

    void $ document ^. js3 ("addEventListener" :: Text) ("load" :: Text) listener True


js_mdcAttach :: Text -> Text -> DOM.Element -> DOM.JSM JSVal
js_mdcAttach mod com e = do
  mdc <- jsg ("mdc" :: Text)
  r <- mdc ^. js mod . js com . js1 ("attachTo" :: Text) e
  e ^. jss ("mdcComponent" :: Text) r
  pure r

mdcAttach :: Text -> Text -> DOM.Element -> DOM.JSM MdcRef
mdcAttach mod com el = MdcRef <$> js_mdcAttach mod com el

js_mdcDetach :: DOM.Element -> DOM.JSM ()
js_mdcDetach e = void (e ^. js ("mdcComponent" :: Text) . js0 ("destroy" :: Text))

----------------------------------------------------------------------------

attachCheckbox :: MaterialWidget t m
               => Maybe (Event t Bool)
               -> Element EventResult (DomBuilderSpace m) t
               -> m ()
attachCheckbox eIndeterminate elm = do
  let elm' = _element_raw elm
  registerAttach (mdcAttach "checkbox" "MDCCheckbox") elm
  case eIndeterminate of
    Just set -> performEvent_ $ liftJSM . setIndeterminate elm' <$> set
    Nothing -> return ()
  return ()

setIndeterminate :: DOM.Element -> Bool -> JSM ()
setIndeterminate elm i = getComponent elm >>= \case
  Just c -> setProp "indeterminate" (toJSBool i) c
  Nothing -> pure ()

getComponent :: DOM.Element -> JSM (Maybe Object)
getComponent elm = do
  component <- elm ^. js ("mdcComponent" :: Text)
  isComponent <- ghcjsPure $ isTruthy component
  if isComponent
    then Just <$> valToObject component
    else pure Nothing

----------------------------------------------------------------------------

attachSelect :: forall t m. MaterialWidget t m
             => Maybe (Event t Int)
             -> Element EventResult (DomBuilderSpace m) t
             -> m (Event t Int)
attachSelect mSetValue elm = do
  let elm' = _element_raw elm

  case mSetValue of
    Just set -> performEvent_ $ liftJSM . setValueMdSelect elm' <$> set
    Nothing -> return ()

  pb <- getMdcLoad
  (eChange, onChange) <- newTriggerEvent

  let
    setup = do
      mdc <- mdcAttach "select" "MDCSelect" elm'
      js_setupSelectChangeListener elm' $ fun $ \_ _ _args -> do
        v <- getValueMdSelect mdc
        liftIO $ onChange v
      pure ()

  {-
  res <- performEvent (liftJSM setup <$ pb)
  switchHold ((-42) <$ pb) res
  -}
  performEvent_ (liftJSM setup <$ pb)
  pure eChange

setValueMdSelect :: DOM.Element -> Int -> JSM ()
setValueMdSelect elm i = getComponent elm >>= \case
  Just comp -> do
    i' <- toJSVal i
    setProp "selectedIndex" i' comp
  Nothing -> pure ()

getValueMdSelect :: MdcRef -> JSM Int
getValueMdSelect (MdcRef val) =
  valToObject val >>= getProp "selectedIndex" >>= fromJSValUnchecked


-- foreign import javascript unsafe
--  "(function(){ $1['addEventListener']('MDCSelect:change', function() { $2($1['mdcComponent']); }); })()"
-- js_setupSelectChangeListener :: DOM.Element -> Callback (JSVal -> DOM.JSM ()) -> DOM.JSM ()
js_setupSelectChangeListener :: DOM.Element -> JSCallAsFunction -> DOM.JSM ()
js_setupSelectChangeListener elm cb = do
  let listener = fun $ \_ this _ -> do
        elm' <- valToObject elm
        comp <- getProp "component" elm'
        cb jsUndefined this [comp]

  void $ elm ^. js2 ("addEventListener" :: Text) ("MDCSelect:change" :: Text) listener
  pure () -- fixme: return a function to clean up listener

----------------------------------------------------------------------------

attachSimpleMenu :: MaterialWidget t m => Event t Bool -> Element EventResult (DomBuilderSpace m) t -> m (Event t Int)
attachSimpleMenu eShow elm = do
  let elm' = _element_raw elm

  performEvent_ $ liftJSM . setMenuOpen elm' . const True <$> eShow

  pb <- getMdcLoad

  (eSelected, onSelected) <- newTriggerEvent
  (eCancel, onCancel) <- newTriggerEvent

  let setup = do
        mdc <- mdcAttach "menu" "MDCSimpleMenu" elm'
        js_setupMenuCancelListener elm' (fun $ \_ _ _ -> liftIO $ onCancel ())

        let selected = fun $ \_ _ [index] -> do
              index' <- fromJSValUnchecked index
              liftIO $ onSelected index'
        js_setupMenuSelectedListener elm' selected

  -- res <- performEvent (liftJSM setup <$ pb)
  -- switchHold ((-42) <$ pb) res
  performEvent_ (liftJSM setup <$ pb)
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
  void $ elm ^. js2 ("addEventListener" :: Text) ("MDCSimpleMenu:selected" :: Text) listener

js_setupMenuCancelListener :: DOM.Element -> JSCallAsFunction -> DOM.JSM ()
js_setupMenuCancelListener elm cb =
  addEventListener ("MDCSimpleMenu:cancel" :: Text) listener
  where
    listener = fun $ \_ this [] -> cb jsUndefined this []
    addEventListener name l = void $ elm ^. js2 ("addEventListener" :: Text) name l
