module Heroes.Requisites (
  with,
  Prov (..),
  Deps (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation
import Animation.Scene                                   (Handle(..))
import Battle.Random                                     (spawn)
import Heroes
import Heroes.Essentials                                 (Essentials(..))
import qualified Battle.Example                            as Example
import qualified Stage.Links                               as L
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
import Test.QuickCheck                                   (generate)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  essentials :: L.Essentials
}

data Prov = Prov {
  initialBattle :: L.InitialBattle,
  setup         :: L.Setup,
  groupSizeOf   :: L.GroupSizeOf
}

--------------------------------------------------------------------------------

with :: Deps -> (Prov -> IO a) -> IO a
with (Deps {..}) next = do
  let groupSizeOf = toGroupSizeOf essentials
  (setup, initialBattle) <-
    if randomizeStart
    then generate $ spawn 5 Example.zero
    else return Example.one
  --
  next $ Prov {..}

toGroupSizeOf :: Essentials -> GroupSizeOf
toGroupSizeOf (Essentials {..}) =
  \h g ->
    case h of
      Handle'Fighter fyr ->
        groupSize (creatureMeta (fyr ^. creature_) ^. groups_) g
      Handle'SFX sfx -> groupSize (sfxMeta sfx ^. groups_) g

groupSize :: V.Vector (V.Vector a) -> GroupNumber -> GroupSize
groupSize gs (GroupNumber g) =
  GroupSize $
    case gs V.!? g of
      Just x -> V.length x
      Nothing -> 0

randomizeStart :: Bool
randomizeStart = False
