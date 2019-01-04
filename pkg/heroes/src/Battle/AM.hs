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
  deriving (Eq, Show)

data PathMarker
  = Move (Placing, Facing, Placing)
  | Turn (Facing, Placing, Facing)
  | Takeoff
  | Landing
  deriving (Eq, Show)

data Update
  = JumpTo Battle
  | Normal [Marker]
