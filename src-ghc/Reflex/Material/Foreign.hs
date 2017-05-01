module Reflex.Material.Foreign where

import Data.Text (Text)
import Reflex.Dom

-- stubs for when we can't use javascript ffi

attachRipple :: MonadWidget t m => El t -> m ()
attachRipple el = return ()

attachTextfield :: MonadWidget t m => El t -> m ()
attachTextfield el = return ()

attachIconToggle :: MonadWidget t m => El t -> m ()
attachIconToggle el = return ()

attachFormField :: MonadWidget t m => El t -> m ()
attachFormField el = return ()

attachCheckbox :: MonadWidget t m => Maybe (Event t Bool) -> El t -> m ()
attachCheckbox _ el = return ()

attachSelect :: MonadWidget t m => Maybe (Event t Int) -> El t -> m (Event t Int)
attachSelect _ el = return never

attachSimpleMenu :: MonadWidget t m => Event t Bool -> El t -> m (Event t Int)
attachSimpleMenu _ el = return never
