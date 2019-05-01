module Battle.AM.Rules where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle.Monad
import Battle.Monad.Utils
import Heroes
import qualified Battle.AM                                 as AM
import Battle                                            (FighterId)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

meleeAttack :: FighterId -> FighterId -> Bearing -> P0
meleeAttack a d b = aMark $ AM.MeleeAttack (a, d, b)

rangeAttack :: FighterId -> FighterId -> P0
rangeAttack a d = aMark $ AM.RangeAttack (a, d)

death :: FighterId -> P0
death f = aMark $ AM.Death f

specialEffect :: SFX -> Facing -> Placing -> P0
specialEffect sfx f p = aMark $ AM.SpecialEffect (sfx, f, p)

takeoff :: FighterId -> P0
takeoff f = aMark $ AM.Path f AM.Takeoff

landing :: FighterId -> P0
landing f = aMark $ AM.Path f AM.Landing

move :: FighterId -> Placing -> P0
move fyr p' = do
  p <- (?!) $ #fighters . by fyr . #placing
  f <- (?!) $ #fighters . by fyr . #facing
  aMark $ AM.Path fyr (AM.Move (p, f, p'))

turn :: FighterId -> Facing -> P0
turn fyr f' = do
  p <- (?!) $ #fighters . by fyr . #placing
  f <- (?!) $ #fighters . by fyr . #facing
  aMark $ AM.Path fyr (AM.Turn (f, p, f'))
