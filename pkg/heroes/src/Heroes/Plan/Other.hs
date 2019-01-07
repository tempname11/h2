module Heroes.Plan.Other where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation
import Animation.Scene
import Heroes
import Heroes.H3.Misc
import Heroes.Plan.Types
import Heroes.SFXResource                                (SFXResource(..))
import Heroes.UI
import Stage.LoadingThread                               (LoadRequest(..))
import qualified Heroes.Plan.Animation                     as Animation
import qualified Heroes.Plan.Sound                         as Sound
import qualified Heroes.UI.Sound                           as Sound
import qualified Heroes.Bearing                            as Bearing
import qualified Heroes.Cell                               as Cell
import qualified Heroes.Placing                            as Placing
import Battle                                            (FighterId)
import Heroes.UI.Sound                                   (Sound(..))
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

(+!) :: Offset -> GroupSize -> Offset
(+!) (Offset o) (GroupSize s) = Offset (o + 4 * s) -- XXX why 4? 15fps -> 60fps?

(+!.) :: Offset -> Int -> Offset
(+!.) (Offset o) i = Offset (o + i)

(><) :: Offset -> Offset -> Offset
(Offset a) >< (Offset b) = Offset (max a b)

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
  Sound.playOnce (Sound'Creature (a ^. creature_) Sound.Attack) o
  Sound.playOnce (Sound'Creature (d ^. creature_) Sound.Defence) o
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
  Sound.playOnce (Sound'Creature (a ^. creature_) Sound.Attack) o
  Sound.playOnce (Sound'Creature (d ^. creature_) Sound.Defence) o
  put $ (oa >< od) +!. 1

specialEffect :: (SFX, Facing, Placing) -> M0
specialEffect (sfx, f, p) = do
  o <- get
  (gso, loaded) <- ask
  sprite <- case (loaded ^. sfxes_) sfx of
    Just (SFXResource { sprite }) -> return sprite
    Nothing -> loadRequest (LoadRequest'SFX sfx)
  let
    h = Handle'SFX sfx
    actor = Actor {
      sprite,
      position = actorPositionAt f p,
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
  Sound.playOnce (Sound'SFX sfx) o
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
  Sound.playOnce (Sound'Creature (fyr ^. creature_) Sound.Death) o
  put o'

obstaclePositionAt :: ObstacleType -> Facing -> Multiplacing -> Position
obstaclePositionAt o _ (Multiplacing hex _) = 
  fieldCenter .+^ Cell.fromHex hex .+^ obstacleOffset o

actorPositionAt :: Facing -> Placing -> Position
actorPositionAt facing placing =
  fieldCenter .+^ Cell.fromHex hex .+^ legsOffset .+^ widthOffset'
  where hex = Placing.base placing
        widthOffset' = if Placing.isWide placing && facing == West
                          then widthOffset
                          else 0

data Group -- XXX creature group
  = Moving
  | Taunting
  | Idling
  | Hitting
  | Defending
  | Dying
  | Turning
  | MeleeAttackingFrom Bearing.Semi

sfxFacing :: Facing
sfxFacing = West

sfxGroupNumber :: GroupNumber
sfxGroupNumber = GroupNumber 0

is :: Group -> GroupNumber
is = GroupNumber . go
  where
  go Moving    = 0
  go Taunting  = 1
  go Idling    = 2
  go Hitting   = 3
  go Defending = 4
  go Dying     = 5
  go Turning   = 8 -- DEF preview says this is 'turn right'
  go (MeleeAttackingFrom Bearing.Up)      = 10
  go (MeleeAttackingFrom Bearing.Forward) = 11
  go (MeleeAttackingFrom Bearing.Down)    = 12
