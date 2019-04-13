module Battle.Setup (
  Setup(..),
  TeamAttr(..),
  PlayerType(..)
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data PlayerType = Human | AI
 deriving (Generic, Show)

data TeamAttr = TeamAttr {
  playerType :: PlayerType
} deriving (Generic, Show)

data Setup = Setup {
  participants :: Map Team TeamAttr,
  field :: Set Hex
} deriving (Generic, Show)
