module Heroes.Sound where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data CType
  = Attack
  | Defence
  | Move
  | Death
  | Wince
  | Shot
  --
  deriving (Eq, Ord, Enum, Bounded, Show)

data Sound
  = Sound'Creature Creature CType
  | Sound'SFX SFX
  --
  deriving (Eq, Ord, Show)

suffix :: CType -> String
suffix Attack  = "ATTK"
suffix Defence = "DFND"
suffix Move    = "MOVE"
suffix Death   = "KILL"
suffix Wince   = "WNCE"
suffix Shot    = "SHOT"

allTypes :: [CType]
allTypes = [minBound .. maxBound]
