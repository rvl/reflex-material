module Reflex.Material.Common
  -- ( MdClassText(..)
  -- , MdHasCustom(..)
  -- , MdHasRole(..)
  -- , MdDensity(..)
  -- , MdRole(..)
  -- ) where
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))

import Reflex.Dom

------------------------------------------------------------------------------
-- | A type class for converting data types into appropriate MDC
-- class text.
class MdClassText a where
  mdText :: a -> Text

------------------------------------------------------------------------------
-- | Passthrough instance for Either
instance (MdClassText a, MdClassText b) => MdClassText (Either a b) where
  mdText (Left a) = mdText a
  mdText (Right b) = mdText b

class MdHasCustom a where
  -- | IMPORTANT: Implementations of this function should use the accompanying
  -- 'addCustom' function to make sure that new values are added on and don't
  -- overwrite anything that was already there.
  custom :: Text -> a -> a

------------------------------------------------------------------------------
-- | Helper function for adding a class item to a custom class field.
addCustom :: Text -> Maybe Text -> Maybe Text
addCustom cls Nothing = Just cls
addCustom cls (Just c) = Just (T.unwords [cls, c])

----------------------------------------------------------------------------

data MdRole
  = MdPrimary
  | MdAccent
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance MdClassText MdRole where
  mdText MdPrimary = "primary"
  mdText MdAccent = "accent"

class MdHasRole a where
  mdSetRole :: MdRole -> a -> a

instance (Reflex t, MdHasRole a) => MdHasRole (Dynamic t a) where
  mdSetRole c = fmap (mdSetRole c)

primary, accent :: MdHasRole a => a -> a
primary = mdSetRole MdPrimary
accent = mdSetRole MdAccent

------------------------------------------------------------------------------

data MdRaised = MdRaised
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance MdClassText MdRaised where
  mdText MdRaised = "raised"

class MdHasRaised a where
  raised :: a -> a

instance (Reflex t, MdHasRaised a) => MdHasRaised (Dynamic t a) where
  raised = fmap raised


------------------------------------------------------------------------------
data MdDark = MdDark
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance MdClassText MdDark where
  mdText MdDark = "dark"

class MdHasDark a where
  dark :: a -> a

instance (Reflex t, MdHasDark a) => MdHasDark (Dynamic t a) where
  dark = fmap dark

------------------------------------------------------------------------------
data MdActive = MdActive
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance MdClassText MdActive where
  mdText MdActive = "active"

class MdHasActive a where
  active :: a -> a

------------------------------------------------------------------------------
data MdDisabled = MdDisabled
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance MdClassText MdDisabled where
  mdText MdDisabled = "disabled"

class MdHasDisabled a where
  disabled :: a -> a

------------------------------------------------------------------------------
data MdSize
  = MdMini
  | MdTiny
  | MdSmall
  | MdMedium
  | MdLarge
  | MdBig
  | MdHuge
  | MdMassive
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance MdClassText MdSize where
  mdText MdMini = "mini"
  mdText MdTiny = "tiny"
  mdText MdSmall = "small"
  mdText MdMedium = "medium"
  mdText MdLarge = "large"
  mdText MdBig = "big"
  mdText MdHuge = "huge"
  mdText MdMassive = "massive"

class MdHasSize a where
  mdSetSize :: MdSize -> a -> a

instance (Reflex t, MdHasSize a) => MdHasSize (Dynamic t a) where
  mdSetSize c = fmap (mdSetSize c)

mini, tiny, small, medium, large, big, huge, massive :: MdHasSize a => a -> a
mini = mdSetSize MdMini
tiny = mdSetSize MdTiny
small = mdSetSize MdSmall
medium = mdSetSize MdMedium
large = mdSetSize MdLarge
big = mdSetSize MdBig
huge = mdSetSize MdHuge
massive = mdSetSize MdMassive


------------------------------------------------------------------------------
data MdDensity = MdCompact | MdDense
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance MdClassText MdDensity where
  mdText MdCompact = "compact"
  mdText MdDense = "dense"

class MdHasDensity a where
  mdSetDensity :: MdDensity -> a -> a

instance (Reflex t, MdHasDensity a) => MdHasDensity (Dynamic t a) where
  mdSetDensity c = fmap (mdSetDensity c)

compact, dense :: MdHasDensity a => a -> a
compact = mdSetDensity MdCompact
dense = mdSetDensity MdDense
