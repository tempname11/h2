{-# LANGUAGE TemplateHaskell #-}
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
}

data Setup = Setup
  { participants :: Map Team TeamAttr
  , field        :: Set Hex
  }

makeShorthands ''TeamAttr
makeShorthands ''Setup
