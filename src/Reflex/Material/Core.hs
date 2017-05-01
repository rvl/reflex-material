module Reflex.Material.Core
  ( attachRipple
  , attachTextfield
  , attachCheckbox
  , attachIconToggle
  , attachFormField
  , attachSelect
  , attachSimpleMenu
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

import Reflex.Dom
import Reflex.Material.Foreign

----------------------------------------------------------------------------
-- fixme: https://github.com/material-components/material-components-web/blob/master/docs/architecture.md
-- fixme: there should probably be a detach/destroy function as well
----------------------------------------------------------------------------
