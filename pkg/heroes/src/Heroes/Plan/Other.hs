module Heroes.Plan.Other where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation
import Animation.Scene
import Battle                                            (FighterId)
import Battle                                            (_creature)
import Heroes
import Heroes.Plan.Common
import Heroes.SFXResource                                (SFXResource(..))
import Heroes.Sound                                      (Sound(..))
import Heroes.UI                                         (widthOffset)
import Stage.LoadingThread                               (LoadRequest(..))
import qualified Heroes.Bearing                            as Bearing
import qualified Heroes.Placing                            as Placing
import qualified Heroes.Plan.Animation                     as Animation
import qualified Heroes.Plan.Sound                         as Sound
import qualified Heroes.Sound                              as Sound
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Monad.State                               (get)
import Control.Monad.State                               (put)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

-- XXX share with as meleeAttack?
rangeAttack :: (FighterId, FighterId) -> M0
rangeAttack (a, d) = do
  o <- get
  (gso, _) <- ask
  let ha = Handle'Fighter a
      hd = Handle'Fighter d
  let oa = o +! gso ha ga
      od = o +! gso hd gd
      ga = is (MeleeAttackingFrom $ Bearing.Forward)
      gd = is Defending
      gi = is Idling
  Animation.setGroupNumber ha ga o
  Animation.setGroupNumber hd gd o
  Animation.setGroupNumber ha gi oa
  Animation.setGroupNumber hd gi od
  let
    sa = (Sound'Creature (a ^. _creature) Sound.Attack)
    sd = (Sound'Creature (d ^. _creature) Sound.Defence)
  ca <- getChunk sa
  cd <- getChunk sd
  Sound.playOnce ha ca o
  Sound.playOnce hd cd o
  put $ (oa >< od) +!. 1

meleeAttack :: (FighterId, FighterId, Bearing) -> M0
meleeAttack (a, d, _bear) = do
  o <- get
  (gso, _) <- ask
  let ha = Handle'Fighter a
      hd = Handle'Fighter d
  let oa = o +! gso ha ga
      od = o +! gso hd gd
      ga = is (MeleeAttackingFrom $ Bearing.Forward) -- XXX Bearing.semi bear
      gd = is Defending
      gi = is Idling
  Animation.setGroupNumber ha ga o
  Animation.setGroupNumber hd gd o
  Animation.setGroupNumber ha gi oa
  Animation.setGroupNumber hd gi od
  let
    sa = (Sound'Creature (a ^. _creature) Sound.Attack)
    sd = (Sound'Creature (d ^. _creature) Sound.Defence)
  ca <- getChunk sa
  cd <- getChunk sd
  Sound.playOnce ha ca o
  Sound.playOnce hd cd o
  put $ (oa >< od) +!. 1

specialEffect :: (SFX, Facing, Placing) -> M0
specialEffect (sfx, f, p) = do
  o <- get
  (gso, loaded) <- ask
  sprite <- case (loaded ^. _sfxes) sfx of
    Just (SFXResource { sprite }) -> return (Some sprite)
    Nothing -> singleRequest (LoadRequest'SFX sfx)
  let
    h = Handle'SFX sfx
    actor = Actor {
      sprite,
      position =
        actorPositionAt East p .+^
          if Placing.isWide p then widthOffset <&> (`div` 2) else 0,
      height = 0,
      facing = f,
      groupN = (\(GroupNumber g) -> g) sfxGroupNumber,
      frameN = 0,
      subframeN = 0,
      animated = True
    }
    o' = o +! gso h sfxGroupNumber
  Animation.add h actor o
  Animation.remove h o'
  c <- getChunk (Sound'SFX sfx)
  Sound.playOnce h c o
  put $ o' +!. 1

death :: FighterId -> M0
death fyr = do
  o <- get
  (gso, _) <- ask
  let h = Handle'Fighter fyr
      g = is Dying
      o' = o +! gso h g
  Animation.setGroupNumber h g o
  Animation.setAnimated h False (o' +!. (-1))
  c <- getChunk (Sound'Creature (fyr ^. _creature) Sound.Death)
  Sound.playOnce h c o
  put o'

sfxFacing :: Facing
sfxFacing = West

sfxGroupNumber :: GroupNumber
sfxGroupNumber = GroupNumber 0
