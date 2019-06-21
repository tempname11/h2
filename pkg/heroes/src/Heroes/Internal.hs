module Heroes.Internal where
-- we don't want to export constructors in the `Heroes` module, so we need to
-- re-export only the types.

import Common

--------------------------------------------------------------------------------

data Hex = Hex !Int !Int
  deriving (Generic, Eq, Ord)

data HexDiff = HexDiff !Int !Int
  deriving (Generic, Eq, Ord, Show)

instance Show Hex where
  show (Hex d q) = "{" <> show d <> "^" <> show q <> "}"

instance Binary Hex
instance Binary HexDiff
