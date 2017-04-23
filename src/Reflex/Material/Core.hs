module Reflex.Material.Core
  ( attachRipple
  , attachTextfield
  , attachCheckbox
  , attachIconToggle
  ) where

import GHCJS.DOM.Element (toElement)
import qualified GHCJS.DOM.Types as GDT
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

import Reflex.Dom

----------------------------------------------------------------------------
-- fixme: https://github.com/material-components/material-components-web/blob/master/docs/architecture.md
-- fixme: there should probably be a detach/destroy function as well
----------------------------------------------------------------------------

registerAttach :: MonadWidget t m => (GDT.Element -> IO ()) -> El t -> m ()
registerAttach attachJS el = registerAttach' attachJS (_element_raw el)

registerAttach' :: MonadWidget t m => (GDT.Element -> IO ()) -> GDT.Element -> m ()
registerAttach' attachJS el = do
  pb <- getPostBuild
  performEvent_ $ (liftIO . attachJS $ el) <$ pb
  -- pd <- getPreDestroy
  -- performEvent_ $ (liftIO . mdcDetactJS $ el) <$ pd

attachRipple :: MonadWidget t m => El t -> m ()
attachRipple = registerAttach (mdcAttachJS "ripple" "MDCRipple")

attachCheckbox :: MonadWidget t m => El t -> m ()
attachCheckbox = registerAttach (mdcAttachJS "checkbox" "MDCCheckbox")

attachTextfield :: MonadWidget t m => El t -> m ()
attachTextfield = registerAttach (mdcAttachJS "textfield" "MDCTextfield")

attachIconToggle :: MonadWidget t m => El t -> m ()
attachIconToggle = registerAttach (mdcAttachJS "iconToggle" "MDCIconToggle")

----------------------------------------------------------------------------
-- fixme: the mdc <script> needs to be hacked into index.html
-- otherwise these functions are called before mdc is ready.

foreign import javascript unsafe "$3.mdcComponent = mdc[$1][$2].attachTo($3);"
  mdcAttachJS :: Text -> Text -> GDT.Element -> IO ()

foreign import javascript unsafe "$1.mdcComponent.destroy();"
  mdcDetachJS :: GDT.Element -> IO ()
