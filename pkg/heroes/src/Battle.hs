{-# LANGUAGE FlexibleInstances #-}
module Battle where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Baked
import Heroes
import Heroes.H3.Misc
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Lens                                      (Lens')
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Failure
  = Invalid
  | Catastrophic

--------------------------------------------------------------------------------

-- Creature is baked in, because that's really convenient
-- and gives us stronger "immutability" guarantees:
-- same id will always refer to the same creature.
newtype FighterId = FighterId Int
  deriving (Generic, Eq, Ord, Show)

instance Baked FighterId where
  type Upper FighterId = Int
  type Lower FighterId = Creature
  lowerBits _ = 8
  wrap = FighterId
  unwrap = \(FighterId x) -> x

makeFighterId :: Creature -> Int -> FighterId
makeFighterId = baked

_creature :: Lens' FighterId Creature
_creature = lower_

--------------------------------------------------------------------------------

newtype ObstacleId = ObstacleId Int
  deriving (Generic, Eq, Ord, Show)

instance Baked ObstacleId where
  type Upper ObstacleId = Int
  type Lower ObstacleId = ObstacleType
  lowerBits _ = 8
  wrap = ObstacleId
  unwrap = \(ObstacleId x) -> x

makeObstacleId :: ObstacleType -> Int -> ObstacleId
makeObstacleId = baked

_otype :: Lens' ObstacleId ObstacleType
_otype = lower_

--------------------------------------------------------------------------------

data Phase
  = Phase'Initial
  | Phase'FighterActionSelection {
    fighter :: FighterId
  }
  | Phase'Movement {
    fighter :: FighterId,
    plane   :: Plane,
    didMove :: Bool,
    points  :: Int
  }
  | Phase'SpellTargetSelection { spell :: Spell }
  | Phase'Terminal
  deriving (Generic, Eq, Ord, Show)

--------------------------------------------------------------------------------

type Spell = SFX

--------------------------------------------------------------------------------

data Ability
  = Ability'Ranged
  | Ability'Flight
  --
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- XXX consider alternative structure (think AoS/SoA)
data FighterAttr = FighterAttr {
  team :: Team,
  speed :: Int,
  attack :: Int,
  defence :: Int,
  abilities :: Set Ability,
  placing :: Placing,
  facing :: Facing
} deriving (Generic, Show)

data BodyAttr = BodyAttr {
  placing :: Placing,
  facing  :: Facing
} deriving (Generic, Show)

data ObstacleAttr = ObstacleAttr {
  multiplacing :: Multiplacing
} deriving (Generic, Show)

--------------------------------------------------------------------------------

data Battle = Battle {
  fighters :: Map FighterId FighterAttr,
  bodies :: Map FighterId BodyAttr,
  obstacles :: Map ObstacleId ObstacleAttr,
  phase :: Phase,
  order :: Stream (Team, Turn)
} deriving (Generic)

--------------------------------------------------------------------------------

-- XXX parametrize type by a (p :: Phase) ?
data Move
  = FighterSelected FighterId
  | SpellSelected Spell
  | MovementSelected
  | RangeAttackSelected FighterId
  | BearingSelected Bearing
  | EOM
  deriving (Eq, Ord, Show)
