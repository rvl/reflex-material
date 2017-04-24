module Reflex.Material.Foreign where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Reflex.Dom
import qualified GHCJS.DOM.Types as GDT

registerAttach :: MonadWidget t m => (GDT.Element -> IO ()) -> El t -> m ()
registerAttach attachJS el = registerAttach' attachJS (_element_raw el)

registerAttach' :: MonadWidget t m => (GDT.Element -> IO ()) -> GDT.Element -> m ()
registerAttach' attachJS el = do
  pb <- getPostBuild
  performEvent_ $ (liftIO . attachJS $ el) <$ pb
  -- pd <- getPreDestroy
  -- performEvent_ $ (liftIO . mdcDetactJS $ el) <$ pd


----------------------------------------------------------------------------
-- fixme: the mdc <script> needs to be hacked into index.html
-- otherwise these functions are called before mdc is ready.

foreign import javascript unsafe "$3.mdcComponent = mdc[$1][$2].attachTo($3);"
  mdcAttachJS :: Text -> Text -> GDT.Element -> IO ()

foreign import javascript unsafe "$1.mdcComponent.destroy();"
  mdcDetachJS :: GDT.Element -> IO ()
