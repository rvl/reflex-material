module Reflex.Material.Foreign where

import Data.Text (Text)
import Reflex.Dom
-- import qualified Graphics.UI.Gtk.WebKit.Types as GDT
import qualified GHCJS.DOM.Types as GDT

registerAttach :: MonadWidget t m => (GDT.Element -> IO ()) -> El t -> m ()
registerAttach _ _ = return ()

mdcAttachJS :: Text -> Text -> GDT.Element -> IO ()
mdcAttachJS _ _ _ = return ()

mdcDetachJS :: GDT.Element -> IO ()
mdcDetachJS _ = return ()
