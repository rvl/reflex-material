{-# LANGUAGE FlexibleContexts #-}

module Reflex.Material.Foreign where

import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Monad (void)
import Data.Text (Text)
import Reflex.Dom
import qualified GHCJS.DOM.Types as GDT
import GHCJS.Types (JSVal)
import GHCJS.DOM.EventTarget
import GHCJS.DOM.EventTargetClosures
import           GHCJS.Foreign.Callback


----------------------------------------------------------------------------
-- Notes on framework integration are here:
--   https://github.com/material-components/material-components-web/blob/master/docs/architecture.md
-- This module uses the simple method of integration. Writing an
-- adapter doesn't seem to be necessary.
----------------------------------------------------------------------------

----------------------------------------------------------------------------
newtype MdcRef = MdcRef { unMdcRef :: JSVal }
----------------------------------------------------------------------------

attachRipple :: MonadWidget t m => El t -> m ()
attachRipple = registerAttach (mdcAttach "ripple" "MDCRipple")

attachTextfield :: MonadWidget t m => El t -> m ()
attachTextfield = registerAttach (mdcAttach "textfield" "MDCTextfield")

attachIconToggle :: MonadWidget t m => El t -> m ()
attachIconToggle = registerAttach (mdcAttach "iconToggle" "MDCIconToggle")

attachFormField :: MonadWidget t m => El t -> m ()
attachFormField = registerAttach (mdcAttach "formField" "MDCFormField")

----------------------------------------------------------------------------

registerAttach :: MonadWidget t m => (GDT.Element -> IO MdcRef) -> El t -> m ()
registerAttach attachJS el = registerAttach' attachJS (_element_raw el)

registerAttach' :: MonadWidget t m => (GDT.Element -> IO MdcRef) -> GDT.Element -> m ()
registerAttach' attachJS el = do
  pb <- getMdcLoad
  performEvent_ $ (liftIO . void . attachJS $ el) <$ pb
  -- fixme: how to clean up?
  -- pd <- getPreDestroy
  -- performEvent_ $ (liftIO . js_mdcDetach $ el) <$ pd

-- | Event which occurs after postBuild and once mdc javascript is loaded.
getMdcLoad :: MonadWidget t m => m (Event t ())
getMdcLoad = do
  pb <- getPostBuild
  let act cb = liftIO $ do
        jscb <- asyncCallback1 $ \_ -> liftIO (cb ())
        js_onLoadMdc jscb
  performEventAsync (act <$ pb)

-- Runs callback when material-components-web.js has loaded.
foreign import javascript unsafe
  "(function(){ if (window.mdc) { $1(window.mdc); } else { var cb = function(evt) { if (evt.target.tagName == 'SCRIPT' && window.mdc) { $1(window.mdc); document.body.removeEventListener('load', cb, true); } }; document.body.addEventListener('load', cb, true); } })()"
  js_onLoadMdc :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$r = mdc[$1][$2].attachTo($3); $3.mdcComponent = $r;"
  js_mdcAttach :: Text -> Text -> GDT.Element -> IO JSVal
mdcAttach :: Text -> Text -> GDT.Element -> IO MdcRef
mdcAttach mod com el = MdcRef <$> js_mdcAttach mod com el

foreign import javascript unsafe "$1.mdcComponent.destroy();"
  js_mdcDetach :: GDT.Element -> IO ()

----------------------------------------------------------------------------

attachCheckbox :: MonadWidget t m => Maybe (Event t Bool) -> El t -> m ()
attachCheckbox eIndeterminate el = do
  let el' = _element_raw el
  registerAttach (mdcAttach "checkbox" "MDCCheckbox") el
  case eIndeterminate of
    Just set -> performEvent_ $ (setIndeterminate el') <$> set
    Nothing -> return ()
  return ()

attachSelect :: MonadWidget t m => Maybe (Event t Int) -> El t -> m (Event t Int)
attachSelect setValue el = do
  let el' = _element_raw el

  case setValue of
    Just set -> performEvent_ $ (setValueMdSelect' el') <$> set
    Nothing -> return ()

  pb <- getMdcLoad
  let act cb = liftIO $ do
        mdc <- mdcAttach "select" "MDCSelect" el'
        jscb <- asyncCallback1 $ \_ -> liftIO $ do
          v <- getValueMdSelect mdc
          cb v
        js_setupSelectChangeListener el' jscb
  performEventAsync (act <$ pb)

attachSimpleMenu :: MonadWidget t m => Event t Bool -> El t -> m (Event t Int)
attachSimpleMenu eShow el = do
  let el' = _element_raw el

  performEvent_ $ (setMenuOpen el' . const True) <$> eShow

  pb <- getMdcLoad
  let act cb = liftIO $ do
        mdc <- mdcAttach "menu" "MDCSimpleMenu" el'
        jscb <- asyncCallback1 $ \ev -> liftIO $ do
          cb 0
        js_setupMenuSelectedListener el' jscb
        js_setupMenuCancelListener el' jscb
  performEventAsync (act <$ pb)

foreign import javascript unsafe
  "(function(){ $1['addEventListener']('MDCSelect:change', function() { $2($1['mdcComponent']); }); })()"
  js_setupSelectChangeListener :: GDT.Element -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "$r = $1['selectedIndex'];"
  js_getSelectedIndex :: MdcRef -> IO Int
getValueMdSelect :: MonadIO m => MdcRef -> m Int
getValueMdSelect a = liftIO $ js_getSelectedIndex a

foreign import javascript unsafe
  "$1['selectedIndex'] = $2;"
  js_setSelectedIndex :: MdcRef -> Int -> IO ()
setValueMdSelect :: MonadIO m => MdcRef -> Int -> m ()
setValueMdSelect mdc i = liftIO $ js_setSelectedIndex mdc i

foreign import javascript unsafe
  "(function() { if ($1['mdcComponent']) { $1['mdcComponent'].selectedIndex = $2; } })()"
  js_setSelectedIndex' :: GDT.Element -> Int -> IO ()
setValueMdSelect' :: MonadIO m => GDT.Element -> Int -> m ()
setValueMdSelect' el i = liftIO $ js_setSelectedIndex' el i

foreign import javascript unsafe
  "(function() { if ($1['mdcComponent']) { $1['mdcComponent'].indeterminate = $2; } })()"
  js_setIndeterminate :: GDT.Element -> Bool -> IO ()
setIndeterminate :: MonadIO m => GDT.Element -> Bool -> m ()
setIndeterminate el i = liftIO $ js_setIndeterminate el i

----------------------------------------------------------------------------

foreign import javascript unsafe
  "(function(){ $1['addEventListener']('MDCSimpleMenu:selected', function(evt) { $2(evt.detail.index); }); })()"
  js_setupMenuSelectedListener :: GDT.Element -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "(function(){ $1['addEventListener']('MDCSimpleMenu:cancel', function() { $2(); }); })()"
  js_setupMenuCancelListener :: GDT.Element -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "(function() { if ($1['mdcComponent']) { $1['mdcComponent'].open = $2; } })()"
  js_setMenuOpen :: GDT.Element -> Bool -> IO ()
setMenuOpen :: MonadIO m => GDT.Element -> Bool -> m ()
setMenuOpen el i = liftIO $ js_setMenuOpen el i
