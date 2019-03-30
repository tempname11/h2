module Battle.Rules (
  makeMove,
  acceptableMoves,
  -- helpers
  currentPlayerType,
  currentFighterPlacing,
  fighterPlacing,
  pNode,
  whoIsOn,
  spawns
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle
import Battle.Monad
import Battle.Monad.Utils
import Battle.Setup
import Common.Hot
import Heroes
import qualified Battle.AM.Rules                           as AM
import qualified Battle.PM                                 as PM
import qualified Heroes.Bearing                            as Bearing
import qualified Heroes.H3                                 as H3
import qualified Heroes.Placing                            as Placing
import qualified Heroes.Hex                                as Hex   
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
import qualified Data.Set                                  as S
import Control.Lens                                      (contains)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data WalkingResult
  = CanAttack Placing Facing Hex FighterId
  | CanMove Placing Facing

--------------------------------------------------------------------------------

acceptableMoves :: (Setup, Battle) -> [Move]
acceptableMoves = unHot . memo1 acceptableMoves' . Hot

acceptableMoves' :: (Setup, Battle) -> [Move]
acceptableMoves' (s, b) =
  case (s, b) #?. allMoves of
    Just ms -> M.keys ms
    Nothing -> []

makeMove :: Move -> P0
makeMove m = do
  ms <- allMoves
  case ms ^.? by m of
    Just p -> p
    Nothing -> invalid

(@@) :: a -> b -> (a, b)
(@@) = (,)

allMoves :: P (Map Move P0)
allMoves = do
  phase <- (?) _phase
  fighters <- (?) _fighters
  --
  let
    allFighters = M.keys fighters
    allSpells = [minBound .. maxBound]
  --
  return $ M.fromList $ case phase of
      --
      Phase'Movement { fighter = fyr, didMove, plane } -> [
          EOM @@ do
            enforce didMove
            placing <- fighterPlacing fyr
            case plane of
              Ground -> return ()
              Aerial -> do
                enforceNoObstacles Ground placing
                AM.landing fyr
            proceed
        ] <> (
          Bearing.list <&> \bearing -> BearingSelected bearing @@ do
            wr <- tryWalking bearing fyr
            case wr of
              --
              CanAttack placing facing hit dfyr -> do -- We are attacking.
                case plane of
                  Ground -> return ()
                  Aerial -> do
                    enforceNoObstacles Ground placing
                    AM.landing fyr
                pointsExhaustedFromAttack
                considerTurning fyr facing
                -- Perform attack.
                pMark $ PM.Attack {
                  attackerPlacing = placing,
                  bearing,
                  hit
                }
                AM.meleeAttack fyr dfyr bearing
                fyr `attacks` dfyr
              --
              CanMove placing facing -> do -- We are walking.
                enforceNoObstacles plane placing
                pointsSpent 1
                considerTurning fyr facing
                -- Perform the actual movement
                walks fyr placing
        )
      Phase'Terminal -> [
          EOM @@ proceed
        ]
      --
      Phase'FighterActionSelection { fighter = fyr } ->
        [
          MovementSelected @@ do
            speed <- (?!) $ _fighters . by fyr . _speed
            canFly <-
              (?!) $ _fighters . by fyr . _abilities . contains Ability'Flight
            when (canFly) $ AM.takeoff fyr
            _phase .= Phase'Movement {
                plane = if canFly then Aerial else Ground,
                fighter = fyr,
                points = speed,
                didMove = False
              }
        ] <> (
          (
            case fighters ^.? by fyr . _abilities . contains Ability'Ranged of
              Just True -> filter (/= fyr) allFighters
              _ -> []
          ) <&> \dfyr -> RangeAttackSelected dfyr @@ do
            p0 <- (?!) $ _fighters . by fyr . _placing
            p1 <- (?!) $ _fighters . by dfyr . _placing
            let rangeAttackRadius = 4 -- XXX
            enforce $
              Placing.distance p0 p1 <= rangeAttackRadius
            considerTurningTowards fyr dfyr
            AM.rangeAttack fyr dfyr
            fyr `attacks` dfyr
        )
      --
      Phase'Initial -> (
          allFighters <&> \fyr -> FighterSelected fyr @@ do
            fighterTeam <- (?!) $ _fighters . by fyr . _team
            enforceM $ (== fighterTeam) <$> currentTeam
            --
            _phase .= Phase'FighterActionSelection { fighter = fyr }
        ) <> (
          allSpells <&> \spell -> SpellSelected spell @@ do
            _phase .= Phase'SpellTargetSelection spell
        )
      --
      Phase'SpellTargetSelection { spell } -> (
          allFighters <&> \fyr -> FighterSelected fyr @@ do
            f <- (?!) $ _fighters . by fyr . _facing
            p <- (?!) $ _fighters . by fyr . _placing
            AM.specialEffect spell f p 
            case spell of
              H3.Haste -> _fighters . by fyr . _speed %=! (+1) -- XXX max 7?
              H3.Slow  -> _fighters . by fyr . _speed %=! (+(-1)) -- XXX min 1?
              H3.Sacrifice -> dies fyr
            _phase .= Phase'Terminal
        )

proceed :: P0
proceed = do
  _order %= nextS
  _phase .= Phase'Initial

-- XXX copy-pasted from placingEmpty
whoIsOn :: Hex -> P (Maybe FighterId)
whoIsOn h = do
  fighters <- (?) $ _fighters
  return $ loop $ M.toList fighters
  where loop :: [(FighterId, FighterAttr)] -> Maybe FighterId
        loop [] = Nothing
        loop ((fyr, u) : us) = if (u ^. _placing) `Placing.has` h
                      then Just fyr
                      else loop us

placingEmpty :: Placing -> P Bool
placingEmpty p = do
  fighters <- (?) $ _fighters
  return $ loop $ M.toList fighters
  where loop :: [(FighterId, FighterAttr)] -> Bool
        loop [] = True
        loop ((_, u) : us) = if (u ^. _placing) `Placing.intersects` p
                      then False
                      else loop us

spawns :: Creature -> FighterAttr -> P0
spawns c attr = do
  fighters <- (?) $ _fighters
  let f = makeFighterId c
          . (+1)
          . maximum
          . (0:)
          . fmap (\(FighterId i) -> i)
          . M.keys
          $ fighters
      placing = attr ^. _placing
  enforceNoObstacles Ground placing
  enforceM $ placingEmpty placing
  _fighters . at f .= Just attr

pointsExhaustedFromAttack :: P ()
pointsExhaustedFromAttack = do
  phase <- (?) $ _phase
  case phase of
    Phase'Movement { points = p, .. } -> do
      enforce $ p >= 0
      _phase .= Phase'Movement { points = -1, didMove = True, .. }
    _ -> invalid

-- check and spend points
pointsSpent :: Int -> P ()
pointsSpent n = do
  phase <- (?) $ _phase
  case phase of
    Phase'Movement { points = p, .. } -> do
      enforce $ p > 0
      _phase .= Phase'Movement { points = p - n, didMove = True, .. }
    _ -> invalid

-- XXX separate melee and range?
attacks :: FighterId -> FighterId -> P0
attacks attacker defender = do
  attack  <- (?!) $ _fighters . by attacker . _attack
  defence <- (?!) $ _fighters . by defender . _defence
  if attack > defence
    then defender & dies
    else _fighters . by defender . _defence -=! 1

walks :: FighterId -> Placing -> P0
walks fyr placing = do
  AM.move fyr placing
  _fighters . by fyr . _placing .=! placing

turns :: FighterId -> Facing -> P0
turns fyr facing = do
  AM.turn fyr facing
  _fighters . by fyr . _facing .=! facing

dies :: FighterId -> P0
dies fyr = do
  AM.death fyr
  FighterAttr {..} <- (?!) $ _fighters . by fyr
  _bodies . at fyr .= Just BodyAttr {..}
  _fighters . at fyr  .= Nothing

enforceNoObstacles :: Plane -> Placing -> P ()
enforceNoObstacles plane p = do
  field <- (?-) $ _field
  case plane of
    Aerial -> return ()
    Ground -> do
      obstacles <- (?) $ _obstacles
      let
        blocked =
          S.unions $
          Hex.fromMulti . view _multiplacing <$> M.elems obstacles
        free = field `S.difference` blocked
      for_ (Placing.visit p) $ \hex -> do
        enforce $ hex `elem` free

-- XXX does not look at obstacles, rename?
tryWalking :: Bearing -> FighterId -> P WalkingResult
tryWalking bearing fyr = do
  --
  placing <- (?!) $ _fighters . by fyr . _placing
  let facing' = Bearing.toFacing bearing
      placing' = Placing.walk bearing placing
      headHex  = Placing.head placing' facing'
      -- where the 'head' will be after the walk.
  fighters <- (?) $ _fighters
  --
  let loopF :: [(FighterId, FighterAttr)] -> Maybe WalkingResult
      loopF [] = Just $ CanMove placing' facing'
      loopF ((ofyr, other) : us) =
        if | ofyr /= fyr ->
             let otherPlacing = other ^. _placing
                 overlap = Placing.overlap placing' otherPlacing
             in case overlap of
               Placing.NoOverlap -> continue
               Placing.OneOverlap hex -> if hex == headHex
                                         then Just $ CanAttack placing facing' hex ofyr
                                         else Nothing
               Placing.TwoOverlaps hexWest hexEast ->
                 let hex = case facing' of
                             West -> hexWest
                             East -> hexEast
                 in Just $ CanAttack placing facing' hex ofyr
           | otherwise -> continue
        where continue = loopF us
  --
  case loopF $ M.toList fighters of
    Just wr -> return wr
    Nothing -> invalid

fighterPlacing :: FighterId -> P Placing
fighterPlacing fyr =
  (?!) $ _fighters . by fyr . _placing

currentFighterPlacing :: P Placing
currentFighterPlacing = do
  phase <- (?) $ _phase
  case phase of
    Phase'FighterActionSelection { fighter = fyr } -> do
      (?!) $ _fighters . by fyr . _placing
    Phase'Movement { fighter = fyr } -> do
      (?!) $ _fighters . by fyr . _placing
    _ -> invalid

pNode :: P PM.Node
pNode = do
  phase <- (?) $ _phase
  (fyr, points) <-
    case phase of
      Phase'Movement { fighter = f, points = p } -> return (f, p)
      _ -> invalid
  --
  placing <- fighterPlacing fyr
  return $ PM.Node placing points

considerTurning :: FighterId -> Facing -> P0
considerTurning fyr facing' = do
  facing <- (?!) $ _fighters . by fyr . _facing
  when (facing /= facing') $ turns fyr facing'

considerTurningTowards :: FighterId -> FighterId -> P0
considerTurningTowards fyr dfyr = do
  a <- (?!) $ _fighters . by fyr . _placing
  d <- (?!) $ _fighters . by dfyr . _placing
  case Placing.preferredFacing a d of
    Just facing -> considerTurning fyr facing
    Nothing -> return ()

currentTeam :: P Team
currentTeam = (fst . currentS) <$> do
  (?) $ _order

currentPlayerType :: P PlayerType
currentPlayerType = do
  t <- currentTeam
  TeamAttr { playerType } <- (?!-) $ _participants . by t
  return playerType

