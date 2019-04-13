module Battle.AM where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle
import Heroes
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Marker
  = MeleeAttack (FighterId, FighterId, Bearing)
  | RangeAttack (FighterId, FighterId)
  | Death FighterId
  | SpecialEffect (SFX, Facing, Placing)
  | Path FighterId PathMarker
  deriving (Generic, Eq, Show)

data PathMarker
  = Move (Placing, Facing, Placing)
  | Turn (Facing, Placing, Facing)
  | Takeoff
  | Landing
  deriving (Generic, Eq, Show)

data Update
  = JumpTo (Some Battle)
  | Normal [Marker]
  deriving (Generic, Show)
