module Battle.Setup (
  Setup(..),
  TeamAttr(..),
  PlayerType(..)
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data PlayerType = Human | AI

data TeamAttr = TeamAttr {
  playerType :: PlayerType
} deriving (Generic)

data Setup = Setup {
  participants :: Map Team TeamAttr,
  field :: Set Hex
} deriving (Generic)
