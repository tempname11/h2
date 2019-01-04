{-# LANGUAGE TemplateHaskell #-}
module Heroes (
  module Heroes.Internal,
  module Fields,
  module Common,
  module Common.Optics,
  Creature,
  SFX,
  Team (..),
  Turn (..),
  Plane (..),
  Facing (..),
  Bearing (..),
  Segment (..),
  Placing (..),
  Multiplacing (..)
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Common.Optics
import Fields
import Heroes.Internal                                   (Hex, HexDiff)
import qualified Heroes.H3                                 as H3
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Set                                           (Set)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

-- convenience file.
-- contains commonly-used names.
-- re-exports Commons

--------------------------------------------------------------------------------

newtype Team = Team Int
  deriving (Eq, Ord, Show)

newtype Turn = Turn Int
  deriving (Eq, Ord, Show, Num)

type Creature = H3.Creature
type SFX = H3.SFX

data Facing
  = West
  | East
  deriving (Eq, Ord, Show, Bounded, Enum)

data Plane
  = Ground
  | Aerial
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

data Bearing -- like "compass bearing"
  = W
  | E
  | NW
  | NE
  | SW
  | SE
  deriving (Eq, Ord, Show)

data Segment = Segment {
  hex :: Hex,
  bearing :: Bearing
} deriving (Eq, Show)

data Placing
  = Narrow Hex
  | Wide Hex
  deriving (Eq, Show)

data Multiplacing = Multiplacing Hex (Set HexDiff)
  deriving (Show)

makeShorthands ''Segment
