module Heroes.Plan (
  Plan,
  make,
  none,
  fromBattle,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation
import Animation.Scene
import Battle
import Heroes
import Heroes.CreatureResource                           (CreatureResource(..))
import Heroes.Plan.Other
import Heroes.Plan.Path
import Heroes.Plan.Types
import Stage.Loading                                     (Loaded)
import Stage.LoadingThread                               (LoadRequest(..))
import qualified Animation.Command                         as Animation
import qualified Battle.AM                                 as AM
import qualified Heroes.UI.Sound                           as Sound
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
import qualified Data.Set                                  as S
import qualified Data.Vector                               as V
import qualified Data.Vector.Mutable                       as MV
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

none :: Plan
none = V.empty

make :: Loaded -> GroupSizeOf -> AM.Update -> Either (Set LoadRequest) Plan
make loaded gso = \case
  AM.JumpTo b -> fromBattle b loaded
  AM.Normal ms -> fromMarkers ms gso loaded

fromBattle ::
  Battle ->
  Loaded ->
  Either (Set LoadRequest) Plan
fromBattle b loaded =
  addActors' <&>
    \addActors ->
      fromList (Offset 10, reset <> fade <> addObstacles <> addActors)
  where
  reset = [(Offset 0, Left Animation.RemoveAll)]
  --
  fade =
    [
      (Offset 0, Left (Animation.SetCurtainOpacity 1.0)),
      (Offset 1, Left (Animation.SetCurtainOpacity 0.9)),
      (Offset 2, Left (Animation.SetCurtainOpacity 0.8)),
      (Offset 3, Left (Animation.SetCurtainOpacity 0.7)),
      (Offset 4, Left (Animation.SetCurtainOpacity 0.6)),
      (Offset 5, Left (Animation.SetCurtainOpacity 0.5)),
      (Offset 6, Left (Animation.SetCurtainOpacity 0.4)),
      (Offset 7, Left (Animation.SetCurtainOpacity 0.3)),
      (Offset 8, Left (Animation.SetCurtainOpacity 0.2)),
      (Offset 9, Left (Animation.SetCurtainOpacity 0.1)),
      (Offset 10, Left (Animation.SetCurtainOpacity 0.0))
    ]
  --
  addObstacles = fmap (atFrame0 . convertO) . M.toList $ b ^. obstacles_
  addActors' = fmap2 atFrame0 . pipe . fmap convertF . M.toList $ b ^. fighters_
  atFrame0 x = (Offset 0, Left x)
  --
  pipe :: -- XXX better naming?
    [Either LoadRequest Animation.Command] ->
    Either (Set LoadRequest) [Animation.Command]
  pipe [] = Right []
  pipe (Left r : xs) =
    case pipe xs of
      Left rs -> Left (S.insert r rs)
      Right _ -> Left (S.singleton r)
  pipe (Right c : xs) =
    case pipe xs of
      Left rs -> Left rs
      Right cs -> Right (c : cs)
  --
  convertO :: (ObstacleId, ObstacleAttr) -> Animation.Command
  convertO (ob, attr) = Animation.PC ob (Animation.PAdd prop)
    where
    prop = Prop {
      position = obstaclePositionAt (ob ^. otype_) East (attr ^. multiplacing_),
      facing = East
    }
  --
  convertF :: (FighterId, FighterAttr) -> Either LoadRequest Animation.Command
  convertF (fyr, attr) =
    actor' <&>
      \actor ->
        Animation.HC (Handle'Fighter fyr) (Animation.Add actor)
    where
      GroupNumber g = is Idling
      c = fyr ^. creature_
      actor' = case (loaded ^. creatures_) c of
        Just (CreatureResource { sprite }) ->
          Right $ Actor {
            sprite,
            position = actorPositionAt (attr ^. facing_) (attr ^. placing_),
            height = 0,
            facing = attr ^. facing_,
            groupN = g,
            frameN = 0,
            subframeN = 0,
            animated = True
          }
        Nothing -> Left (LoadRequest'Creature c)

fromMarkers ::
  [AM.Marker] ->
  GroupSizeOf ->
  Loaded ->
  Either (Set LoadRequest) Plan
fromMarkers ms gso loaded =
  fmap fromList $
    runWriterT $
      flip execStateT (Offset 0) $
        flip runReaderT (gso, loaded) $
          fromMarkers' ms

fromList :: (Offset, [(Offset, Either Animation.Command Sound.Command)]) -> Plan
fromList (Offset n, cs) = V.create $ do
  mv <- MV.replicate (n + 1) (V.empty, V.empty) -- XXX get rid of "+ 1"
  for_ cs $ \(Offset i, c) ->
    case c of
      Left a -> MV.modify mv (over _1 (V.cons a)) i
      Right s -> MV.modify mv (over _2 (V.cons s)) i
  return mv

fromMarkers' :: [AM.Marker] -> M0
fromMarkers' [] = return ()
fromMarkers' (m : ms) = case m of
  AM.MeleeAttack x -> do
    meleeAttack x
    fromMarkers' ms
  AM.RangeAttack x -> do
    rangeAttack x
    fromMarkers' ms
  AM.Death x -> do
    death x
    fromMarkers' ms
  AM.SpecialEffect x -> do
    specialEffect x
    fromMarkers' ms
  AM.Path uc p -> do
    let (ps, ms') = consecutivePath uc ms
    path uc (p : ps)
    fromMarkers' ms'

consecutivePath :: FighterId -> [AM.Marker] -> ([AM.PathMarker], [AM.Marker])
consecutivePath uc ms =
  case ms of
    (AM.Path uc' p) : ms'
      | uc == uc' ->
        let (ps, ms'') = consecutivePath uc ms'
        in (p:ps, ms'')
    _ -> ([], ms)

