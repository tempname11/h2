module Heroes.Common where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import qualified Heroes.H3                                 as H3
import Heroes.Internal                                   (Hex, HexDiff)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Bearing -- like "compass bearing"
  = W
  | E
  | NW
  | NE
  | SW
  | SE
  deriving (Generic, Eq, Ord, Show)

newtype Team = Team Int
  deriving (Generic, Eq, Ord, Show)

newtype Turn = Turn Int
  deriving (Generic, Eq, Ord, Show, Num)

type Creature = H3.Creature
type SFX = H3.SFX

data Facing
  = West
  | East
  deriving (Generic, Eq, Ord, Show, Bounded, Enum)

data Plane
  = Ground
  | Aerial
  deriving (Generic, Eq, Ord, Show)

-- XXX DeriveAnyClass (since ghc 8.2)
instance GEnum Bearing
instance GEnum Plane

data Segment = Segment {
  hex :: Hex,
  bearing :: Bearing
} deriving (Generic, Eq, Show)

data Placing
  = Narrow Hex
  | Wide Hex
  deriving (Generic, Eq, Show)

data Multiplacing = Multiplacing Hex (Set HexDiff)
  deriving (Generic, Show)

data Annotation
  = Annotation'MeleeFrom Bearing
  | Annotation'Range
  | Annotation'Running
  | Annotation'Pondering
  | Annotation'Selecting
  | Annotation'Thinking -- XXX does not belong here
  deriving (Eq, Ord, Show)
