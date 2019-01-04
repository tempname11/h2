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
  = CanAttack Facing Hex FighterId
  | CanMove Facing Placing

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
  phase <- (?) phase_
  fighters <- (?) fighters_
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
            AM.landing fyr
            case plane of
              Ground -> return ()
              Aerial -> enforcePlane Ground placing
            proceed
        ] <> (
          Bearing.list <&> \bearing -> BearingSelected bearing @@ do
            result <- tryWalking plane bearing fyr
            case result of
              --
              CanAttack facing hit dfyr -> do -- We are attacking.
                pointsExhaustedFromAttack
                considerTurning fyr facing
                -- Perform attack.
                pMark $ PM.Attack bearing hit
                AM.meleeAttack fyr dfyr bearing
                fyr `attacks` dfyr
              --
              CanMove facing newPlacing -> do -- We are walking.
                pointsSpent 1
                considerTurning fyr facing
                -- Perform the actual movement
                walks fyr newPlacing
        )
      Phase'Terminal -> [
          EOM @@ proceed
        ]
      --
      Phase'FighterActionSelection { fighter = fyr } ->
        [
          MovementSelected @@ do
            speed <- (?!) $ fighters_ . by fyr . speed_
            plane <- do
              canFly <-
                (?!) $ fighters_ . by fyr . abilities_ . contains Ability'Flight
              return $ if canFly then Aerial else Ground
            AM.takeoff fyr
            phase_ .= Phase'Movement {
                plane,
                fighter = fyr,
                points = speed,
                didMove = False
              }
        ] <> (
          (
            case fighters ^.? by fyr . abilities_ . contains Ability'Ranged of
              Just True -> filter (/= fyr) allFighters
              _ -> []
          ) <&> \dfyr -> RangeAttackSelected dfyr @@ do
            p0 <- (?!) $ fighters_ . by fyr . placing_
            p1 <- (?!) $ fighters_ . by dfyr . placing_
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
            fighterTeam <- (?!) $ fighters_ . by fyr . team_
            enforceM $ (== fighterTeam) <$> currentTeam
            --
            phase_ .= Phase'FighterActionSelection { fighter = fyr }
        ) <> (
          allSpells <&> \spell -> SpellSelected spell @@ do
            phase_ .= Phase'SpellTargetSelection spell
        )
      --
      Phase'SpellTargetSelection { spell } -> (
          allFighters <&> \fyr -> FighterSelected fyr @@ do
            f <- (?!) $ fighters_ . by fyr . facing_
            p <- (?!) $ fighters_ . by fyr . placing_
            AM.specialEffect spell f p 
            case spell of
              H3.Haste -> fighters_ . by fyr . speed_ %=! (+1) -- XXX max 7?
              H3.Slow  -> fighters_ . by fyr . speed_ %=! (+(-1)) -- XXX min 1?
              H3.Sacrifice -> dies fyr
            phase_ .= Phase'Terminal
        )

proceed :: P0
proceed = do
  order_ %= nextS
  phase_ .= Phase'Initial

-- XXX copy-pasted from placingEmpty
whoIsOn :: Hex -> P (Maybe FighterId)
whoIsOn h = do
  fighters <- (?) $ fighters_
  return $ loop $ M.toList fighters
  where loop :: [(FighterId, FighterAttr)] -> Maybe FighterId
        loop [] = Nothing
        loop ((fyr, u) : us) = if (u ^. placing_) `Placing.has` h
                      then Just fyr
                      else loop us

placingEmpty :: Placing -> P Bool
placingEmpty p = do
  fighters <- (?) $ fighters_
  return $ loop $ M.toList fighters
  where loop :: [(FighterId, FighterAttr)] -> Bool
        loop [] = True
        loop ((_, u) : us) = if (u ^. placing_) `Placing.intersects` p
                      then False
                      else loop us

spawns :: Creature -> FighterAttr -> P0
spawns c attr = do
  fighters <- (?) $ fighters_
  let f = makeFighterId c
          . (+1)
          . maximum
          . (0:)
          . fmap (\(FighterId i) -> i)
          . M.keys
          $ fighters
      placing = attr ^. placing_
  enforcePlane Ground placing
  enforceM $ placingEmpty placing
  fighters_ . at f .= Just attr

pointsExhaustedFromAttack :: P ()
pointsExhaustedFromAttack = do
  phase <- (?) $ phase_
  case phase of
    Phase'Movement { points = p, .. } -> do
      enforce $ p >= 0
      phase_ .= Phase'Movement { points = -1, didMove = True, .. }
    _ -> invalid

-- check and spend points
pointsSpent :: Int -> P ()
pointsSpent n = do
  phase <- (?) $ phase_
  case phase of
    Phase'Movement { points = p, .. } -> do
      enforce $ p > 0
      phase_ .= Phase'Movement { points = p - n, didMove = True, .. }
    _ -> invalid

-- XXX separate melee and range?
attacks :: FighterId -> FighterId -> P0
attacks attacker defender = do
  attack  <- (?!) $ fighters_ . by attacker . attack_
  defence <- (?!) $ fighters_ . by defender . defence_
  if attack > defence
    then defender & dies
    else fighters_ . by defender . defence_ -=! 1

walks :: FighterId -> Placing -> P0
walks fyr placing = do
  AM.move fyr placing
  fighters_ . by fyr . placing_ .=! placing

turns :: FighterId -> Facing -> P0
turns fyr facing = do
  AM.turn fyr facing
  fighters_ . by fyr . facing_ .=! facing

dies :: FighterId -> P0
dies fyr = do
  AM.death fyr
  FighterAttr {..} <- (?!) $ fighters_ . by fyr
  bodies_ . at fyr .= Just BodyAttr {..}
  fighters_ . at fyr  .= Nothing

enforcePlane :: Plane -> Placing -> P ()
enforcePlane plane p = do
  field <- (?-) $ field_
  obstacles <-
    case plane of
      Ground -> (?) $ obstacles_
      Aerial -> return empty
  let
    blocked =
      S.unions $
      Hex.fromMulti . view multiplacing_ <$> M.elems obstacles
    free = field `S.difference` blocked
  for_ (Placing.visit p) $ \hex -> do
    enforce $ hex `elem` free

tryWalking :: Plane -> Bearing -> FighterId -> P WalkingResult
tryWalking plane bearing fyr = do
  --
  placing <- (?!) $ fighters_ . by fyr . placing_
  let facing' = Bearing.toFacing bearing
      placing' = Placing.walk bearing placing
      headHex  = Placing.head placing' facing'
      -- where the 'head' will be after the walk.
  fighters <- (?) $ fighters_
  --
  let loopF :: [(FighterId, FighterAttr)] -> Maybe WalkingResult
      loopF [] = Just $ CanMove facing' placing'
      loopF ((ofyr, other) : us) =
        if | ofyr /= fyr ->
             let otherPlacing = other ^. placing_
                 overlap = Placing.overlap placing' otherPlacing
             in case overlap of
               Placing.NoOverlap -> continue
               Placing.OneOverlap hex -> if hex == headHex
                                         then Just $ CanAttack facing' hex ofyr
                                         else Nothing
               Placing.TwoOverlaps hexWest hexEast ->
                 let hex = case facing' of
                             West -> hexWest
                             East -> hexEast
                 in Just $ CanAttack facing' hex ofyr
           | otherwise -> continue
        where continue = loopF us
  --
  case loopF $ M.toList fighters of
    Just wr@(CanMove {}) -> do
      enforcePlane plane placing'
      return wr
    Just wr@(CanAttack {}) -> do
      -- XXX wrong place?
      AM.landing fyr
      enforcePlane Ground placing
      return wr
    Nothing -> invalid

fighterPlacing :: FighterId -> P Placing
fighterPlacing fyr =
  (?!) $ fighters_ . by fyr . placing_

currentFighterPlacing :: P Placing
currentFighterPlacing = do
  phase <- (?) $ phase_
  case phase of
    Phase'FighterActionSelection { fighter = fyr } -> do
      (?!) $ fighters_ . by fyr . placing_
    Phase'Movement { fighter = fyr } -> do
      (?!) $ fighters_ . by fyr . placing_
    _ -> invalid

pNode :: P PM.Node
pNode = do
  phase <- (?) $ phase_
  (fyr, points) <-
    case phase of
      Phase'Movement { fighter = f, points = p } -> return (f, p)
      _ -> invalid
  --
  placing <- fighterPlacing fyr
  let hex = Placing.base placing
  return $ PM.Node hex points

considerTurning :: FighterId -> Facing -> P0
considerTurning fyr facing' = do
  facing <- (?!) $ fighters_ . by fyr . facing_
  when (facing /= facing') $ turns fyr facing'

considerTurningTowards :: FighterId -> FighterId -> P0
considerTurningTowards fyr dfyr = do
  a <- (?!) $ fighters_ . by fyr . placing_
  d <- (?!) $ fighters_ . by dfyr . placing_
  case Placing.preferredFacing a d of
    Just facing -> considerTurning fyr facing
    Nothing -> return ()

currentTeam :: P Team
currentTeam = (fst . currentS) <$> do
  (?) $ order_

currentPlayerType :: P PlayerType
currentPlayerType = do
  t <- currentTeam
  TeamAttr { playerType } <- (?!-) $ participants_ . by t
  return playerType

