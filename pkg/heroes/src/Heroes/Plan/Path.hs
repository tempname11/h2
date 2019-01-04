module Heroes.Plan.Path where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation.Scene
import Heroes
import Heroes.Plan.Other
import Heroes.Plan.Types
import qualified Battle.AM                                 as AM
import qualified Heroes.Plan.Animation                     as Animation
import qualified Heroes.Plan.Sound                         as Sound
import qualified Heroes.UI.Sound                           as Sound
import Battle                                            (FighterId)
import Heroes.UI.Sound                                   (Sound(..))
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

path :: FighterId -> [AM.PathMarker] -> Sh
path fyr ps gso o = do
  let h = Handle'Fighter fyr
  o' <- path' False fyr ps gso o
  Sound.start (Sound'Creature (fyr ^. creature_) Sound.Move) o
  Sound.stop  (Sound'Creature (fyr ^. creature_) Sound.Move) o'
  Animation.setGroupNumber h (is Idling) o'
  return o'

path' :: Bool -> FighterId -> [AM.PathMarker] -> Sh
path' prevMove fyr ps gso o =
  case ps of
    [] -> return o
    AM.Move x : ps' -> do
      o' <- handleMove prevMove fyr x gso o
      path' True fyr ps' gso o'
    AM.Turn (_, p, f) : ps' -> do
      o' <- handleTurn fyr (f, p) gso o
      path' False fyr ps' gso o'
    AM.Takeoff : ps' -> do
      o' <- handleFlight prevMove 0 flightHeight fyr gso o
      path' True fyr ps' gso o'
    AM.Landing : ps' -> do
      o' <- handleFlight prevMove flightHeight 0 fyr gso o
      path' False fyr ps' gso o'

lerp' :: Float -> Position -> Position -> Position
lerp' w b a = round <$> lerp w ((<§>) b) ((<§>) a)

framesPerMove :: Int
framesPerMove = 60

flightHeight :: CInt
flightHeight = 50 -- eyeballed

handleFlight :: Bool -> CInt -> CInt -> FighterId -> Sh
handleFlight prevMove a b fyr _ o = do
  let h = Handle'Fighter fyr
      g = is Moving
      m = framesPerMove
  when (not prevMove) $
    Animation.setGroupNumber h g o
  for_ [1..m] $ \i -> do
    let o' = o +!. i
        w :: Double
        w = (§) i / (§) m
        z :: CInt
        z = round . (\(V1 x) -> x) $ lerp w (V1 $ (§) b) (V1 $ (§) a)
    Animation.setHeight h z o'
  return (o +!. m)

handleMove :: Bool -> FighterId -> (Placing, Facing, Placing) -> Sh
handleMove prevMove fyr (p0, f, p1) _ o = do
  let h = Handle'Fighter fyr
      g = is Moving
      m = framesPerMove
  when (not prevMove) $
    Animation.setGroupNumber h g o
  let a = actorPositionAt f p0
      b = actorPositionAt f p1
  for_ [1..m] $ \i -> do
    let o' = o +!. i
        w = (§) i / (§) m
        z = lerp' w b a
    Animation.setPosition h z o'
  return (o +!. m)

handleTurn :: FighterId -> (Facing, Placing) -> Sh
handleTurn fyr (f, p) gso o = do
  let h = Handle'Fighter fyr
      g = is Turning
      o' = o +! gso h g
      z = actorPositionAt f p
  Animation.setGroupNumber h g o
  Animation.setFacing h f o'
  Animation.setPosition h z o'
  return o'
