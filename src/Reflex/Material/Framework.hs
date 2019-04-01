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
  , attachMenuSurface
  , attachFloatingLabel
  , attachLineRipple
  , MaterialWidget
  ) where

import Prelude hiding ((!!))

import Control.Lens ((^.))
import Data.Text (Text)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Say

import qualified Data.JSString as JSString
import Language.Javascript.JSaddle (JSVal, js, js0, js1, js2, js3, jss, jsg, fun, function, JSCallAsFunction, isTruthy, jsUndefined, ghcjsPure, strictEqual, setProp, toJSBool, valToObject, Object, toJSVal, getProp, fromJSValUnchecked, (!), (!!), MakeArgs, jsf, jsNull)
import qualified GHCJS.DOM.Types as DOM
import GHCJS.DOM.Types (JSM, MonadJSM(..), liftJSM)
import GHCJS.DOM.Element as DOM (getElementsByTagName)

import Reflex.Dom.Core

import Reflex.Material.Common (MaterialWidget)

----------------------------------------------------------------------------
-- Notes on framework integration are here:
--   https://github.com/material-components/material-components-web/blob/master/docs/integrating-into-frameworks.md
-- This module uses the simple method of integration. Writing an
-- adapter doesn't seem to be necessary.
----------------------------------------------------------------------------

----------------------------------------------------------------------------
newtype ComponentRef = ComponentRef { unComponentRef :: JSVal }
newtype MdcRef = MdcRef { unMdcRef :: JSVal }
----------------------------------------------------------------------------

attachRipple :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachRipple = registerAttach (mdcAttach "ripple" "MDCRipple")

attachLineRipple :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachLineRipple = registerAttach (mdcAttach "lineRipple" "MDCLineRipple")

attachFloatingLabel :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachFloatingLabel = registerAttach (mdcAttach "floatingLabel" "MDCFloatingLabel")

attachTextfield :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachTextfield = registerAttach (mdcAttach "textField" "MDCTextField")

attachIconToggle :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachIconToggle = registerAttach (mdcAttach "iconToggle" "MDCIconToggle")

attachFormField
  :: MaterialWidget t m
  => Element EventResult (DomBuilderSpace m) t -- ^ The form field element
  -> Maybe (Element EventResult (DomBuilderSpace m) t) -- ^ The embedded input element
  -> m ()
attachFormField formField input = do
  let setup elm mdc = do
        component <- mdcAttach "formField" "MDCFormField" elm mdc
        maybe (pure ()) (setFormFieldInput component . _element_raw) input
  registerAttach' setup (_element_raw formField)

attachMenuSurface :: MaterialWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
attachMenuSurface = registerAttach (mdcAttach "menuSurface" "MDCMenuSurface")

----------------------------------------------------------------------------

registerAttach
  :: (MaterialWidget t m)
  => (DOM.Element -> MdcRef -> DOM.JSM a)
  -> Element EventResult (DomBuilderSpace m) t
  -> m ()
registerAttach attachJS = registerAttach' attachJS . _element_raw

consoleLog :: (MakeArgs args) => args -> JSM ()
consoleLog args = do
  w <- jsg ("console" :: Text)
  w ^. jsf ("log" :: Text) args
  pure ()

registerAttach'
  :: (MaterialWidget t m, MonadJSM (Performable m), PostBuild t m)
  => (DOM.Element -> MdcRef -> JSM a) -> DOM.Element -> m ()
registerAttach' attachJS elm = do
  liftJSM $ do
    w <- jsg ("console" :: Text)
    w ^. js2 ("log" :: Text) ("registerAttach" :: Text) elm
  mdc <- getMdcLoad
  performEvent_ ((liftJSM . void . attachJS elm) <$> mdc)
  pure ()
  -- fixme: how to clean up?
  -- pd <- getPreDestroy
  -- performEvent_ $ (liftJSM . js_mdcDetach $ el) <$ pd


-- | Event which occurs after postBuild and once mdc javascript is loaded.
getMdcLoad :: MaterialWidget t m => m (Event t MdcRef)
getMdcLoad = do
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


mdcAttach :: Text -> Text -> DOM.Element -> MdcRef -> DOM.JSM ComponentRef
mdcAttach mod com e (MdcRef mdc) = do
  r <- mdc ^. js mod . js com . js1 ("attachTo" :: Text) e
  e ^. jss ("mdcComponent" :: Text) r
  pure $ ComponentRef r

js_mdcDetach :: DOM.Element -> DOM.JSM ()
js_mdcDetach e = void (e ^. js ("mdcComponent" :: Text) . js0 ("destroy" :: Text))

----------------------------------------------------------------------------

attachCheckbox :: MaterialWidget t m
               => Maybe (Event t Bool)
               -> Element EventResult (DomBuilderSpace m) t
               -> m ()
attachCheckbox eIndeterminate elm = do
  registerAttach (mdcAttach "checkbox" "MDCCheckbox") elm
  case eIndeterminate of
    Just set -> performEvent_ $ liftJSM . setIndeterminate (_element_raw elm) <$> set
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

  liftIO $ say "attachSelect 1"

  case mSetValue of
    Just set -> performEvent_ $ liftJSM . setValueMdSelect elm' <$> set
    Nothing -> return ()

  liftIO $ say "attachSelect 2"

  mdcInit <- getMdcLoad
  (eChange, onChange) <- newTriggerEvent

  let
    setup mdc = do
      liftIO $ say "setup"
      component <- mdcAttach "select" "MDCSelect" elm' mdc
      liftIO $ say "got mdc"
      js_setupSelectChangeListener elm' $ fun $ \_ _ _args -> do
        liftIO $ say "select change listener fired"
        v <- getValueMdSelect component
        liftIO $ onChange v
      pure ()

  liftIO $ say "attachSelect 3"

  performEvent_ (liftJSM . setup <$> mdcInit)
  liftIO $ say "attachSelect 4"
  pure eChange

setValueMdSelect :: DOM.Element -> Int -> JSM ()
setValueMdSelect elm i = do
  liftIO $ sayString $ "setValueMdSelect " ++ show i
  getComponent elm >>= \case
    Just comp -> do
      liftIO $ say "got a component"
      i' <- toJSVal i
      setProp "selectedIndex" i' comp
    Nothing -> pure ()

getValueMdSelect :: ComponentRef -> JSM Int
getValueMdSelect (ComponentRef val) = do
  liftIO $ say "getValueMdSelect"
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

  liftIO $ say "adding listener"
  void $ elm ^. js2 ("addEventListener" :: Text) ("MDCSelect:change" :: Text) listener
  pure () -- fixme: return a function to clean up listener

----------------------------------------------------------------------------

attachSimpleMenu :: MaterialWidget t m => Event t Bool -> Element EventResult (DomBuilderSpace m) t -> m (Event t Int)
attachSimpleMenu eShow elm = do
  let elm' = _element_raw elm

  performEvent_ $ liftJSM . setMenuOpen elm' . const True <$> eShow

  mdcInit <- getMdcLoad

  (eSelected, onSelected) <- newTriggerEvent
  (eCancel, onCancel) <- newTriggerEvent

  let setup mdc = do
        void $ mdcAttach "menu" "MDCSimpleMenu" elm' mdc
        js_setupMenuCancelListener elm' (fun $ \_ _ _ -> liftIO $ onCancel ())

        let selected = fun $ \_ _ [index] -> do
              index' <- fromJSValUnchecked index
              liftIO $ onSelected index'
        js_setupMenuSelectedListener elm' selected

  performEvent_ (liftJSM . setup <$> mdcInit)
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

----------------------------------------------------------------------------

-- | Connects a MDC Form Field with its inner component element.
setFormFieldInput :: ComponentRef -> DOM.Element -> JSM ()
setFormFieldInput ffComponent cb = do
  cbComponent <- cb ^. js ("mdcComponent" :: Text)
  void $ unComponentRef ffComponent ^. jss ("input" :: Text) cbComponent
