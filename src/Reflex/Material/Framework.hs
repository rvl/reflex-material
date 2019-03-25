module Reflex.Material.Framework
  ( attachRipple
  , attachTextfield
  , attachCheckbox
  , attachIconToggle
  , attachFormField
  , attachSelect
  , attachSimpleMenu
  ) where

-- import Reflex.Material.Foreign

import qualified GHCJS.DOM.Types as DOM
import           GHCJS.DOM.Types (JSM(..), liftJSM, MonadJSM)
import           Reflex.Dom.Core
import Control.Lens ((^.))
import Data.Text (Text)
import Control.Monad (void)

import           Language.Javascript.JSaddle.Object (js, js0, js1, jsg1, jss, jsg)
import           Language.Javascript.JSaddle (JSVal)

----------------------------------------------------------------------------
-- stubs for when we can't use javascript ffi

attachCheckbox :: forall t m. DomBuilder t m => Maybe (Event t Bool) -> Element EventResult (DomBuilderSpace m) t -> m ()
attachCheckbox _ el = return ()

attachSelect :: forall t m. DomBuilder t m => Maybe (Event t Int) -> Element EventResult (DomBuilderSpace m) t -> m (Event t Int)
attachSelect _ el = return never

attachSimpleMenu :: forall t m. DomBuilder t m => Event t Bool -> Element EventResult (DomBuilderSpace m) t -> m (Event t Int)
attachSimpleMenu _ el = return never

----------------------------------------------------------------------------


----------------------------------------------------------------------------
-- Notes on framework integration are here:
--   https://github.com/material-components/material-components-web/blob/master/docs/architecture.md
-- This module uses the simple method of integration. Writing an
-- adapter doesn't seem to be necessary.
----------------------------------------------------------------------------

----------------------------------------------------------------------------
newtype MdcRef = MdcRef { unMdcRef :: JSVal }
----------------------------------------------------------------------------

attachRipple :: (DomBuilder t m, PerformEvent t m, MonadJSM m) => Element EventResult (DomBuilderSpace m) t -> m ()
attachRipple = registerAttach (mdcAttach "ripple" "MDCRipple")

attachTextfield :: (DomBuilder t m, PerformEvent t m, MonadJSM m) => Element EventResult (DomBuilderSpace m) t -> m ()
attachTextfield = registerAttach (mdcAttach "textfield" "MDCTextfield")

attachIconToggle :: (DomBuilder t m, PerformEvent t m, MonadJSM m) => Element EventResult (DomBuilderSpace m) t -> m ()
attachIconToggle = registerAttach (mdcAttach "iconToggle" "MDCIconToggle")

attachFormField :: (DomBuilder t m, PerformEvent t m, MonadJSM m) => Element EventResult (DomBuilderSpace m) t -> m ()
attachFormField = registerAttach (mdcAttach "formField" "MDCFormField")

----------------------------------------------------------------------------

registerAttach ::
  ( DomBuilder t m
  , PerformEvent t m
  , MonadJSM m)
  => (DOM.Element -> DOM.JSM MdcRef)
  -> Element EventResult (DomBuilderSpace m) t
  -> m ()
registerAttach attachJS el = registerAttach' attachJS (_element_raw el)

registerAttach' :: (DomBuilder t m, PerformEvent t m, MonadJSM m) => (DOM.Element -> JSM MdcRef) -> DOM.Element -> m ()
registerAttach' attachJS el = do
  pb <- getMdcLoad
  -- performEvent_ ((liftJSM . void . attachJS $ el) <$ pb)
  pure ()

  -- fixme: how to clean up?
  -- pd <- getPreDestroy
  -- performEvent_ $ (liftJSM . js_mdcDetach $ el) <$ pd

-- | Event which occurs after postBuild and once mdc javascript is loaded.
getMdcLoad :: DomBuilder t m => m (Event t ())
getMdcLoad = do
  {-
  pb <- getPostBuild
  let act cb = liftJSM $ do
        jscb <- asyncCallback1 $ \_ -> DOM.liftJSM (cb ())
        js_onLoadMdc jscb
  performEventAsync (act <$ pb)
  -}
  pure never

{-
-- Runs callback when material-components-web.js has loaded.
foreign import javascript unsafe
  "(function(){ if (window.mdc) { $1(window.mdc); } else { var cb = function(evt) { if (evt.target.tagName == 'SCRIPT' && window.mdc) { $1(window.mdc); document.removeEventListener('load', cb, true); } }; document.addEventListener('load', cb, true); } })()"
  js_onLoadMdc :: Callback (JSVal -> DOM.JSM ()) -> DOM.JSM ()
-}


-- foreign import javascript unsafe "$r = mdc[$1][$2].attachTo($3); $3.mdcComponent = $r;"
js_mdcAttach :: Text -> Text -> DOM.Element -> DOM.JSM JSVal
js_mdcAttach mod com e = do
  mdc <- jsg ("mdc" :: Text)
  r <- mdc ^. js mod . js com . js1 ("attachTo" :: Text) e
  e ^. jss ("mdcComponent" :: Text) r
  pure r

mdcAttach :: Text -> Text -> DOM.Element -> DOM.JSM MdcRef
mdcAttach mod com el = MdcRef <$> js_mdcAttach mod com el

-- foreign import javascript unsafe "$1.mdcComponent.destroy();"
js_mdcDetach :: DOM.Element -> DOM.JSM ()
js_mdcDetach e = void (e ^. js ("mdcComponent" :: Text) . js0 ("destroy" :: Text))

----------------------------------------------------------------------------

{-
attachCheckbox :: DomBuilder t m => Maybe (Event t Bool) -> Element EventResult (DomBuilderSpace m) t -> m ()
attachCheckbox eIndeterminate el = do
  let el' = _element_raw el
  registerAttach (mdcAttach "checkbox" "MDCCheckbox") el
  case eIndeterminate of
    Just set -> performEvent_ $ (setIndeterminate el') <$> set
    Nothing -> return ()
  return ()

attachSelect :: DomBuilder t m => Maybe (Event t Int) -> Element EventResult (DomBuilderSpace m) t -> m (Event t Int)
attachSelect setValue el = do
  let el' = _element_raw el

  case setValue of
    Just set -> performEvent_ $ (setValueMdSelect' el') <$> set
    Nothing -> return ()

  pb <- getMdcLoad
  let act cb = liftJSM $ do
        mdc <- mdcAttach "select" "MDCSelect" el'
        jscb <- asyncCallback1 $ \_ -> liftJSM $ do
          v <- getValueMdSelect mdc
          cb v
        js_setupSelectChangeListener el' jscb
  performEventAsync (act <$ pb)

attachSimpleMenu :: DomBuilder t m => Event t Bool -> Element EventResult (DomBuilderSpace m) t -> m (Event t Int)
attachSimpleMenu eShow el = do
  let el' = _element_raw el

  performEvent_ $ (setMenuOpen el' . const True) <$> eShow

  pb <- getMdcLoad
  let act cb = liftJSM $ do
        mdc <- mdcAttach "menu" "MDCSimpleMenu" el'
        jscb <- asyncCallback1 $ \ev -> liftJSM $ do
          cb 0
        js_setupMenuSelectedListener el' jscb
        js_setupMenuCancelListener el' jscb
  performEventAsync (act <$ pb)

foreign import javascript unsafe
  "(function(){ $1['addEventListener']('MDCSelect:change', function() { $2($1['mdcComponent']); }); })()"
  js_setupSelectChangeListener :: DOM.Element -> Callback (JSVal -> DOM.JSM ()) -> DOM.JSM ()

foreign import javascript unsafe
  "$r = $1['selectedIndex'];"
  js_getSelectedIndex :: MdcRef -> DOM.JSM Int
getValueMdSelect :: MdcRef -> JSM Int
getValueMdSelect a = liftJSM $ js_getSelectedIndex a

foreign import javascript unsafe
  "$1['selectedIndex'] = $2;"
  js_setSelectedIndex :: MdcRef -> Int -> DOM.JSM ()
setValueMdSelect :: MonadIO m => MdcRef -> Int -> m ()
setValueMdSelect mdc i = liftJSM $ js_setSelectedIndex mdc i

foreign import javascript unsafe
  "(function() { if ($1['mdcComponent']) { $1['mdcComponent'].selectedIndex = $2; } })()"
  js_setSelectedIndex' :: DOM.Element -> Int -> DOM.JSM ()
setValueMdSelect' :: MonadIO m => DOM.Element -> Int -> m ()
setValueMdSelect' el i = liftJSM $ js_setSelectedIndex' el i

foreign import javascript unsafe
  "(function() { if ($1['mdcComponent']) { $1['mdcComponent'].indeterminate = $2; } })()"
  js_setIndeterminate :: DOM.Element -> Bool -> DOM.JSM ()
setIndeterminate :: MonadIO m => DOM.Element -> Bool -> m ()
setIndeterminate el i = liftJSM $ js_setIndeterminate el i

----------------------------------------------------------------------------

foreign import javascript unsafe
  "(function(){ $1['addEventListener']('MDCSimpleMenu:selected', function(evt) { $2(evt.detail.index); }); })()"
  js_setupMenuSelectedListener :: DOM.Element -> Callback (JSVal -> DOM.JSM ()) -> DOM.JSM ()

foreign import javascript unsafe
  "(function(){ $1['addEventListener']('MDCSimpleMenu:cancel', function() { $2(); }); })()"
  js_setupMenuCancelListener :: DOM.Element -> Callback (JSVal -> DOM.JSM ()) -> DOM.JSM ()

foreign import javascript unsafe
  "(function() { if ($1['mdcComponent']) { $1['mdcComponent'].open = $2; } })()"
  js_setMenuOpen :: DOM.Element -> Bool -> DOM.JSM ()
setMenuOpen :: MonadIO m => DOM.Element -> Bool -> m ()
setMenuOpen el i = liftJSM $ js_setMenuOpen el i
-}
