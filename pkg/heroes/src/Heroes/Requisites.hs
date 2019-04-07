module Heroes.Requisites (
  with,
  Prov (..),
  Deps (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation
import Animation.Scene                                   (Handle(..))
import Battle                                            (Battle)
import Battle                                            (_creature)
import Battle.Random                                     (spawn)
import Battle.Setup                                      (Setup)
import Heroes
import Heroes.Essentials                                 (Essentials(..))
import qualified Battle.Example                            as Example
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Test.QuickCheck                                   (generate)
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  essentials :: Essentials
}

data Prov = Prov {
  groupSizeOf :: GroupSizeOf,
  initialBattle :: Battle,
  setup :: Setup
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
        groupSize (creatureMeta (fyr ^. _creature) ^. _groups) g
      Handle'SFX sfx -> groupSize (sfxMeta sfx ^. _groups) g

groupSize :: V.Vector (V.Vector a) -> GroupNumber -> GroupSize
groupSize gs (GroupNumber g) =
  GroupSize $
    case gs V.!? g of
      Just x -> V.length x
      Nothing -> 0

randomizeStart :: Bool
randomizeStart = True
