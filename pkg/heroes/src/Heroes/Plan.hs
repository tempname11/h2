module Heroes.Plan (
  Plan,
  make,
  none,
  fromBattle,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Battle
import Heroes.Plan.Types
import Heroes.Plan.Other
import Heroes.Plan.Path
import Animation
import Animation.Scene

import qualified Animation.Command                         as Animation
import qualified Battle.AM                                 as AM
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
import qualified Data.Vector.Mutable                       as MV
import qualified Data.IntMap.Strict                        as IM
import qualified Data.Map.Strict                           as M
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

{-
data S a where
  Append :: (Handle, Command) -> S ()
  GetGroupSizeOf :: Handle -> GroupNumber -> S GroupSize
  Sequentially :: [S a] ->
-}

none :: Plan
none = V.empty

make :: GroupSizeOf -> AM.Update -> Plan
make gso = \case
  AM.JumpTo b -> fromBattle b
  AM.Normal ms -> fromMarkers ms gso

fromBattle :: Battle -> Plan
fromBattle b = fromIntMap im
  where
  im :: IntMap Cmds
  im = IM.fromList [
      (0, (
        os <>
        cs <>
        [Animation.RemoveAll, Animation.SetCurtainOpacity 1.0], []
      )),
      (1, ([Animation.SetCurtainOpacity 0.9], [])),
      (2, ([Animation.SetCurtainOpacity 0.8], [])),
      (3, ([Animation.SetCurtainOpacity 0.7], [])),
      (4, ([Animation.SetCurtainOpacity 0.6], [])),
      (5, ([Animation.SetCurtainOpacity 0.5], [])),
      (6, ([Animation.SetCurtainOpacity 0.4], [])),
      (7, ([Animation.SetCurtainOpacity 0.3], [])),
      (8, ([Animation.SetCurtainOpacity 0.2], [])),
      (9, ([Animation.SetCurtainOpacity 0.1], [])),
      (10,([Animation.SetCurtainOpacity 0.0], []))
    ]
  --
  os :: [Animation.Command]
  os = map convertO . M.toList $ b ^. obstacles_
  --
  convertO :: (ObstacleId, ObstacleAttr) -> Animation.Command
  convertO (ob, attr) = Animation.PC ob (Animation.PAdd prop)
    where
    prop = Prop {
      position = obstaclePositionAt (ob ^. otype_) East (attr ^. multiplacing_),
      facing = East
    }
  --
  cs :: [Animation.Command]
  cs = map convertF . M.toList $ b ^. fighters_
  --
  convertF :: (FighterId, FighterAttr) -> Animation.Command
  convertF (fyr, attr) = Animation.HC (Handle'Fighter fyr) (Animation.Add actor)
    where
      actor = Actor {
        position = actorPositionAt (attr ^. facing_) (attr ^. placing_),
        height = 0,
        facing = attr ^. facing_,
        groupN = g,
        frameN = 0,
        subframeN = 0,
        animated = True
      }
      GroupNumber g = is Idling

fromMarkers :: [AM.Marker] -> GroupSizeOf -> Plan
fromMarkers ms gso =
  fromIntMap $
    flip execState empty $
      fromMarkers' ms gso (Offset 0)

fromIntMap :: IntMap Cmds -> Plan
fromIntMap im = V.create $ do
  let n = if IM.null im
          then 0
          -- XXX why is the "1 +" below?
          else 1 + fst (IM.findMax im) -- findMax is partial :(
  mv <- MV.replicate n ([], [])
  for_ (IM.toList im) $ \(k, m) ->
    MV.write mv k m
  return mv

fromMarkers' :: [AM.Marker] -> GroupSizeOf -> Offset -> S0
fromMarkers' [] _ _ = return ()
fromMarkers' (m : ms) gso o = case m of
  AM.MeleeAttack x -> do
    o' <- meleeAttackSh x gso o
    fromMarkers' ms gso o'
  AM.RangeAttack x -> do
    o' <- rangeAttackSh x gso o
    fromMarkers' ms gso o'
  AM.Death x -> do
    o' <- death x gso o
    fromMarkers' ms gso o'
  AM.SpecialEffect x -> do
    o' <- specialEffect x gso o
    fromMarkers' ms gso o'
  AM.Path uc p -> do
    let (ps, ms') = consecutivePath uc ms
    o' <- path uc (p:ps) gso o
    fromMarkers' ms' gso o'

consecutivePath :: FighterId -> [AM.Marker] -> ([AM.PathMarker], [AM.Marker])
consecutivePath uc ms =
  case ms of
    (AM.Path uc' p) : ms'
      | uc == uc' ->
        let (ps, ms'') = consecutivePath uc ms'
        in (p:ps, ms'')
    _ -> ([], ms)

