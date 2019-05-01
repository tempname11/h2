module Battle.Rules (
  makeMove,
  acceptableMovesMemo,
  acceptableMoves',
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
import Heroes
import qualified Battle.AM.Rules                           as AM
import qualified Battle.PM                                 as PM
import qualified Common.Hot                                as Hot
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

acceptableMovesMemo :: Current (Setup, Battle) -> [Move]
acceptableMovesMemo =
  Hot.forget .
  Hot.memo1 (\(Current c) -> acceptableMoves' c) .
  Hot.currently

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
  phase <- (?) #phase
  fighters <- (?) #fighters
  --
  let
    allFighters = M.keys fighters
  --
  return $ M.fromList $ case phase of
      --
      Phase'Movement { fighter = fyr, didMove, plane, points } -> [
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
                considerTurning fyr facing
                pMark $ PM.Attack {
                  attackerPlacing = placing,
                  bearing,
                  hit
                }
                AM.meleeAttack fyr dfyr bearing
                fyr `attacks` dfyr
                #phase .= Phase'Movement {
                  points = 0,
                  didMove = True,
                  fighter = fyr,
                  plane = Ground
                }
              --
              CanMove placing facing -> do -- We are walking.
                enforce $ points > 0
                enforceNoObstacles plane placing
                considerTurning fyr facing
                AM.move fyr placing
                #fighters . by fyr . #placing .=! placing
                #phase .= Phase'Movement {
                  points = points - 1,
                  didMove = True,
                  fighter = fyr,
                  plane
                }
        )
      Phase'Terminal -> [
          EOM @@ proceed
        ]
      --
      Phase'FighterActionSelection { fighter = fyr } ->
        [
          MovementSelected @@ do
            speed <- (?!) $ #fighters . by fyr . #speed
            canFly <-
              (?!) $ #fighters . by fyr . #abilities . contains Ability'Flight
            when (canFly) $ AM.takeoff fyr
            #phase .= Phase'Movement {
                plane = if canFly then Aerial else Ground,
                fighter = fyr,
                points = speed,
                didMove = False
              }
        ] <> (
          (
            case fighters ^.? by fyr . #abilities . contains Ability'Ranged of
              Just True -> filter (/= fyr) allFighters
              _ -> []
          ) <&> \dfyr -> RangeAttackSelected dfyr @@ do
            p0 <- (?!) $ #fighters . by fyr . #placing
            p1 <- (?!) $ #fighters . by dfyr . #placing
            let rangeAttackRadius = 4 -- XXX
            enforce $
              Placing.distance p0 p1 <= rangeAttackRadius
            considerTurningTowards fyr dfyr
            AM.rangeAttack fyr dfyr
            fyr `attacks` dfyr
            #phase .= Phase'Terminal
        )
      --
      Phase'Initial -> (
          allFighters <&> \fyr -> FighterSelected fyr @@ do
            fighterTeam <- (?!) $ #fighters . by fyr . #team
            enforceM $ (== fighterTeam) <$> currentTeam
            --
            #phase .= Phase'FighterActionSelection { fighter = fyr }
        ) <> (
          genum <&> \spell -> SpellSelected spell @@ do
            #phase .= Phase'SpellTargetSelection spell
        )
      --
      Phase'SpellTargetSelection { spell } -> (
          allFighters <&> \fyr -> FighterSelected fyr @@ do
            f <- (?!) $ #fighters . by fyr . #facing
            p <- (?!) $ #fighters . by fyr . #placing
            AM.specialEffect spell f p 
            case spell of
              H3.SFX'Haste -> #fighters . by fyr . #speed %=! (+1) -- XXX max 7?
              H3.SFX'Slow  -> #fighters . by fyr . #speed %=! (+(-1)) -- XXX min 1?
              --H3.Sacrifice -> dies fyr
            #phase .= Phase'Terminal
        )

proceed :: P0
proceed = do
  #order %= nextS
  #phase .= Phase'Initial

-- XXX copy-pasted from placingEmpty
whoIsOn :: Hex -> P (Maybe FighterId)
whoIsOn h = do
  fighters <- (?) $ #fighters
  return $ loop $ M.toList fighters
  where loop :: [(FighterId, FighterAttr)] -> Maybe FighterId
        loop [] = Nothing
        loop ((fyr, u) : us) = if (u ^. #placing) `Placing.has` h
                      then Just fyr
                      else loop us

placingEmpty :: Placing -> P Bool
placingEmpty p = do
  fighters <- (?) $ #fighters
  return $ loop $ M.toList fighters
  where loop :: [(FighterId, FighterAttr)] -> Bool
        loop [] = True
        loop ((_, u) : us) = if (u ^. #placing) `Placing.intersects` p
                      then False
                      else loop us

spawns :: Creature -> FighterAttr -> P0
spawns c attr = do
  fighters <- (?) $ #fighters
  let f = makeFighterId c
          . (+1)
          . maximum
          . (0:)
          . fmap (\(FighterId i) -> i)
          . M.keys
          $ fighters
      placing = attr ^. #placing
  enforceNoObstacles Ground placing
  enforceM $ placingEmpty placing
  #fighters . at f .= Just attr

-- XXX separate melee and range?
attacks :: FighterId -> FighterId -> P0
attacks attacker defender = do
  attack  <- (?!) $ #fighters . by attacker . #attack
  defence <- (?!) $ #fighters . by defender . #defence
  if attack > defence
    then defender & dies
    else #fighters . by defender . #defence -=! 1

turns :: FighterId -> Facing -> P0
turns fyr facing = do
  AM.turn fyr facing
  #fighters . by fyr . #facing .=! facing

dies :: FighterId -> P0
dies fyr = do
  AM.death fyr
  FighterAttr {..} <- (?!) $ #fighters . by fyr
  #bodies . at fyr .= Just BodyAttr {..}
  #fighters . at fyr  .= Nothing

enforceNoObstacles :: Plane -> Placing -> P ()
enforceNoObstacles plane p = do
  field <- (?-) $ #field
  case plane of
    Aerial -> return ()
    Ground -> do
      obstacles <- (?) $ #obstacles
      let
        blocked =
          S.unions $
          Hex.fromMulti . view #multiplacing <$> M.elems obstacles
        free = field `S.difference` blocked
      for_ (Placing.visit p) $ \hex -> do
        enforce $ hex `elem` free

-- XXX does not look at obstacles, rename?
tryWalking :: Bearing -> FighterId -> P WalkingResult
tryWalking bearing fyr = do
  --
  placing <- (?!) $ #fighters . by fyr . #placing
  let facing' = Bearing.toFacing bearing
      placing' = Placing.walk bearing placing
      headHex  = Placing.head placing' facing'
      -- where the 'head' will be after the walk.
  fighters <- (?) $ #fighters
  --
  let loopF :: [(FighterId, FighterAttr)] -> Maybe WalkingResult
      loopF [] = Just $ CanMove placing' facing'
      loopF ((ofyr, other) : us) =
        if | ofyr /= fyr ->
             let otherPlacing = other ^. #placing
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
  (?!) $ #fighters . by fyr . #placing

currentFighterPlacing :: P Placing
currentFighterPlacing = do
  phase <- (?) $ #phase
  case phase of
    Phase'FighterActionSelection { fighter = fyr } -> do
      (?!) $ #fighters . by fyr . #placing
    Phase'Movement { fighter = fyr } -> do
      (?!) $ #fighters . by fyr . #placing
    _ -> invalid

pNode :: P PM.Node
pNode = do
  phase <- (?) $ #phase
  (fyr, points) <-
    case phase of
      Phase'Movement { fighter = f, points = p } -> return (f, p)
      _ -> invalid
  --
  placing <- fighterPlacing fyr
  return $ PM.Node placing points

considerTurning :: FighterId -> Facing -> P0
considerTurning fyr facing' = do
  facing <- (?!) $ #fighters . by fyr . #facing
  when (facing /= facing') $ turns fyr facing'

considerTurningTowards :: FighterId -> FighterId -> P0
considerTurningTowards fyr dfyr = do
  a <- (?!) $ #fighters . by fyr . #placing
  d <- (?!) $ #fighters . by dfyr . #placing
  case Placing.preferredFacing a d of
    Just facing -> considerTurning fyr facing
    Nothing -> return ()

currentTeam :: P Team
currentTeam = (fst . currentS) <$> do
  (?) $ #order

currentPlayerType :: P PlayerType
currentPlayerType = do
  t <- currentTeam
  TeamAttr { playerType } <- (?!-) $ #participants . by t
  return playerType

