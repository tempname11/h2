module Heroes.Internal where
-- we don't want to export constructors in the `Heroes` module, so we need to
-- re-export only the types.

import Common

--------------------------------------------------------------------------------

-- XXX pack
data Hex = Hex !Int !Int
  deriving (Eq, Ord, Show)

data HexDiff = HexDiff !Int !Int
  deriving (Eq, Ord, Show)
