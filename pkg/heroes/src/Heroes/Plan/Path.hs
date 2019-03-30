module Heroes.Plan.Path where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation.Scene
import Battle                                            (_creature)
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

path :: FighterId -> [AM.PathMarker] -> M0
path fyr ps = do
  o <- get
  path' False fyr ps
  o' <- get
  Sound.start (Sound'Creature (fyr ^. _creature) Sound.Move) o
  Sound.stop  (Sound'Creature (fyr ^. _creature) Sound.Move) (o' +!. (-1))
  let h = Handle'Fighter fyr
  Animation.setGroupNumber h (is Idling) o'
  put (o' +!. 1)

path' :: Bool -> FighterId -> [AM.PathMarker] -> M0
path' prevMove fyr ps = do
  case ps of
    [] -> return ()
    AM.Move x : ps' -> do
      handleMove prevMove fyr x
      path' True fyr ps'
    AM.Turn (_, p, f) : ps' -> do
      handleTurn fyr (f, p)
      path' False fyr ps'
    AM.Takeoff : ps' -> do
      handleFlight prevMove 0 flightHeight fyr
      path' True fyr ps'
    AM.Landing : ps' -> do
      handleFlight prevMove flightHeight 0 fyr
      path' False fyr ps'

lerp' :: Float -> Position -> Position -> Position
lerp' w b a = round <$> lerp w ((<§>) b) ((<§>) a)

framesPerMove :: Int
framesPerMove = 60

flightHeight :: CInt
flightHeight = 50 -- eyeballed

handleFlight :: Bool -> CInt -> CInt -> FighterId -> M0
handleFlight prevMove a b fyr = do
  o <- get
  let h = Handle'Fighter fyr
      g = is Moving
      m = framesPerMove
  when (not prevMove) $
    Animation.setGroupNumber h g o
  for_ [0 .. m - 1] $ \i -> do
    let o' = o +!. i
        w :: Double
        w = (§) i / (§) m
        z :: CInt
        z = round . (\(V1 x) -> x) $ lerp w (V1 $ (§) b) (V1 $ (§) a)
    Animation.setHeight h z o'
  put (o +!. m)

handleMove :: Bool -> FighterId -> (Placing, Facing, Placing) -> M0
handleMove prevMove fyr (p0, f, p1) = do
  o <- get
  let h = Handle'Fighter fyr
      g = is Moving
      m = framesPerMove
  when (not prevMove) $
    Animation.setGroupNumber h g o
  let a = actorPositionAt f p0
      b = actorPositionAt f p1
  for_ [0 .. m - 1] $ \i -> do
    let o' = o +!. i
        w = (§) i / (§) m
        z = lerp' w b a
    Animation.setPosition h z o'
  put (o +!. m)

handleTurn :: FighterId -> (Facing, Placing) -> M0
handleTurn fyr (f, p) = do
  o <- get
  (gso, _) <- ask
  let h = Handle'Fighter fyr
      g = is Turning
      o' = o +! gso h g
      z = actorPositionAt f p
  Animation.setGroupNumber h g o
  Animation.setFacing h f o'
  Animation.setPosition h z o'
  put (o' +!. 1)
