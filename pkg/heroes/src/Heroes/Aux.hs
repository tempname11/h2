{-# LANGUAGE FlexibleContexts #-}
module Heroes.Aux where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle
import Battle.Movement
import Battle.Monad.Utils
import Battle.Rules
import Battle.Setup
import Heroes
import qualified Common.Hot                                as Hot
import qualified Heroes.Hex                                as Hex
import qualified Heroes.Bearing                            as Bearing
import qualified Heroes.Placing                            as Placing
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
import qualified Data.Set                                  as S
import Data.Maybe                                        (isJust)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

-- TODO remove
data Aux = Aux
  { movementHexes :: Movement
  , selectionHexes :: Map Hex (Move, Placing, Annotation)
  , selectableFighters :: Set FighterId
  } deriving (Generic)

--------------------------------------------------------------------------------

approximate :: Bearing -> (Bearing -> Maybe a) -> Maybe a
approximate b0 f = foldl choose Nothing (Bearing.closest b0)
  where
  choose c b = c <|> f b

payloadAt :: Segment -> Aux -> Maybe (Annotation, [Move], Maybe Placing)
payloadAt
  (Segment hex b0)
  Aux { movementHexes = M p c, selectionHexes = s }
  --
  = peaceful <|> conflict <|> selecting
  where
  peaceful, conflict, selecting :: Maybe (Annotation, [Move], Maybe Placing)
  peaceful =
    case M.lookup hex p of
      Just (MR { moves, destinationPlacing }) ->
        Just (Annotation'Running, moves, Just destinationPlacing)
      Nothing -> Nothing
  conflict =
    approximate b0 $ \b ->
      case M.lookup (b, hex) c of
        Just (MR { moves, destinationPlacing }) -> 
          Just (Annotation'MeleeFrom b, moves, Just destinationPlacing)
        Nothing -> Nothing
  selecting =
    M.lookup hex s <&>
      (\(i, _, a) -> (a, [i], Nothing))

aux :: Current (Setup, Battle) -> Aux
aux = Hot.forget . Hot.memo1 aux' . Hot.currently

aux' :: Current (Setup, Battle) -> Aux
aux' current@(Hot.Current (setup, battle)) = Aux {
    movementHexes = processedMovement (setup, battle),
    selectionHexes = M.fromList $ concat $ fmap f1 selectables,
    selectableFighters = S.fromList $ map f2 selectables
  }
  where
  f1 (x, _) = fmap (, x) (Placing.visit (view _2 x))
  f2 (_, x) = x
  selectables = mapMaybe sel . filter isGood $ acceptableMovesMemo current
    where
    isGood m = isJust ((setup, battle) #?. makeMove m)
    sel :: Move -> Maybe ((Move, Placing, Annotation), FighterId)
    sel m = case m of
      FighterSelected fyr -> do
        p <- (setup, battle) #?. fighterPlacing fyr
        let a = Annotation'Selecting
        return ((m, p, a), fyr)
      RangeAttackSelected fyr -> do
        p <- (setup, battle) #?. fighterPlacing fyr
        let a = Annotation'Range
        return ((m, p, a), fyr)
      _ -> Nothing

processedMovement :: (Setup, Battle) -> Movement
processedMovement (setup, battle) =
  case (setup, battle) #?.! do
    currentFighterPlacing
  of
    Just (placing, battle') ->
      let
        M peaceful conflict = movement [MovementSelected] (setup, battle')
      in
        case placing of
          Wide _ ->
            let
              notInBase h _ = not $ h `elem` (Placing.visit placing)
              peacefulShifted = M.mapKeys (Hex.to E) peaceful
              peacefulUnion =
                M.unionWith
                  (\
                    a@(MR { pointsLeft = pA })
                    b@(MR { pointsLeft = pB }) ->
                    if pA >= pB then a else b
                  )
                  peaceful
                  peacefulShifted
             in
               M (M.filterWithKey notInBase peacefulUnion) conflict
          Narrow _ -> M peaceful conflict
    Nothing -> M empty empty
