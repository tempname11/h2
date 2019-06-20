module Heroes.Handle where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Baked
import Common
import qualified Heroes.H3                                 as H3
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Handle
  = Handle'Fighter FighterId
  | Handle'SFX H3.SFX
  deriving (Eq, Ord, Show)

-- Creature is baked in, because that's really convenient
-- and gives us stronger "immutability" guarantees:
-- same id will always refer to the same creature.
newtype FighterId = FighterId Int
  deriving (Generic, Eq, Ord, Show)

instance Baked FighterId where
  type Upper FighterId = Int
  type Lower FighterId = H3.Creature
  lowerBits _ = 8
  wrap = FighterId
  unwrap = \(FighterId x) -> x

makeFighterId :: H3.Creature -> Int -> FighterId
makeFighterId = baked

_creature :: Lens' FighterId H3.Creature
_creature = lower_
