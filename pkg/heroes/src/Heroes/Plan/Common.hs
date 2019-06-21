module Heroes.Plan.Common where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation
import Animation.Scene
import Heroes
import Heroes.Essentials                                 (Essentials)
import Heroes.H3.Misc                                    (ObstacleType)
import Heroes.H3.Misc                                    (obstacleOffset)
import Heroes.UI
import Heroes.Sound                                      (Sound(..))
import Stage.Loading                                     (Loaded)
import Stage.LoadingThread                               (LoadRequest(..))
import qualified Animation.Command                         as Animation
import qualified Heroes.Bearing                            as Bearing
import qualified Heroes.Cell                               as Cell
import qualified Heroes.Placing                            as Placing
import qualified Heroes.SND                                as SND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Set                                  as S
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype Offset = Offset Int
  deriving (Show)
type M0 = M ()
type Plan = V.Vector (V.Vector Animation.Command, V.Vector SND.Command)

type M =
  ReaderT (Essentials, Loaded) (
    StateT Offset (
      WriterT [(Offset, Either Animation.Command SND.Command)] (
        Either (Set LoadRequest)
      )
    )
  )

singleRequest :: LoadRequest -> M a
singleRequest r = lift . lift . lift $ Left (S.singleton r)

getChunk :: Sound -> M SND.Chunk
getChunk sound = do
  (_, loaded) <- ask
  case sound of
    Sound'Creature c t ->
      case (loaded ^. #creatures) c of
        Just r -> 
          case r ^. #sounds . at t of
            Just x -> return x
            Nothing -> singleRequest (LoadRequest'Creature c)
        Nothing -> singleRequest (LoadRequest'Creature c)
    Sound'SFX s ->
      case (loaded ^. #sfxes) s of
        Just x -> return (x ^. #chunk)
        Nothing -> singleRequest (LoadRequest'SFX s)

(+!) :: Offset -> GroupSize -> Offset
(+!) (Offset o) (GroupSize s) = Offset (o + 4 * s) -- XXX why 4? 15fps -> 60fps?

(+!.) :: Offset -> Int -> Offset
(+!.) (Offset o) i = Offset (o + i)

(><) :: Offset -> Offset -> Offset
(Offset a) >< (Offset b) = Offset (max a b)

data Group -- XXX creature group
  = Moving
  | Taunting
  | Idling
  | Hitting
  | Defending
  | Dying
  | Turning
  | MeleeAttackingFrom Bearing.Semi

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
