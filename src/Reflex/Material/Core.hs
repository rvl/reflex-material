module Reflex.Material.Core
  ( attachRipple
  , attachTextfield
  , attachCheckbox
  , attachIconToggle
  , attachFormField
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

import Reflex.Dom
import Reflex.Material.Foreign

----------------------------------------------------------------------------
-- fixme: https://github.com/material-components/material-components-web/blob/master/docs/architecture.md
-- fixme: there should probably be a detach/destroy function as well
----------------------------------------------------------------------------

attachRipple :: MonadWidget t m => El t -> m ()
attachRipple = registerAttach (mdcAttachJS "ripple" "MDCRipple")

attachCheckbox :: MonadWidget t m => El t -> m ()
attachCheckbox = registerAttach (mdcAttachJS "checkbox" "MDCCheckbox")

attachTextfield :: MonadWidget t m => El t -> m ()
attachTextfield = registerAttach (mdcAttachJS "textfield" "MDCTextfield")

attachIconToggle :: MonadWidget t m => El t -> m ()
attachIconToggle = registerAttach (mdcAttachJS "iconToggle" "MDCIconToggle")

attachFormField :: MonadWidget t m => El t -> m ()
attachFormField = registerAttach (mdcAttachJS "formField" "MDCFormField")
