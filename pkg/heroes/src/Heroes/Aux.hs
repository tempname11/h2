{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Heroes.Aux where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle
import Battle.Movement
import Battle.Monad.Utils
import Battle.Rules
import Battle.Setup
import Common.Hot
import Heroes
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
  }

data Annotation
  = MeleeAttackingFrom Bearing
  | Running
  | Pondering
  | Selecting
  | RangeAttacking
  deriving (Eq, Ord, Show)

makeShorthands ''Aux

--------------------------------------------------------------------------------

approximate :: Bearing -> (Bearing -> Maybe a) -> Maybe (Bearing, a)
approximate b0 f = foldl choose Nothing (Bearing.closest b0)
  where
  choose c b = c <|> (b,) <$> f b

payloadAt :: Segment -> Aux -> Maybe (Annotation, [Move])
payloadAt
  (Segment hex b0)
  Aux { movementHexes = M p c, selectionHexes = s }
  --
  = peaceful <|> conflict <|> selecting
  where
  peaceful, conflict, selecting :: Maybe (Annotation, [Move])
  peaceful = (Running,) . fst <$> M.lookup hex p
  conflict =
    over _1 MeleeAttackingFrom <$>
      (approximate b0 $ \b -> fst <$> M.lookup (b, hex) c)
  selecting = M.lookup hex s <&> (\(i, _, a) -> (a, [i]))

-- TODO memoize, split
aux :: Current (Setup, Battle) -> Aux
aux (Current (setup, battle)) = Aux {
    movementHexes = processedMovement (setup, battle),
    selectionHexes = M.fromList $ concat $ fmap f1 selectables,
    selectableFighters = S.fromList $ map f2 selectables
  }
  where
  f1 (x, _) = fmap (, x) (Placing.visit (view _2 x))
  f2 (_, x) = x
  selectables = mapMaybe sel . filter isGood $ acceptableMoves (setup, battle)
    where
    isGood m = isJust ((setup, battle) #?. makeMove m)
    sel :: Move -> Maybe ((Move, Placing, Annotation), FighterId)
    sel m = case m of
      FighterSelected fyr -> do
        p <- (setup, battle) #?. fighterPlacing fyr
        let a = Selecting
        return ((m, p, a), fyr)
      RangeAttackSelected fyr -> do
        p <- (setup, battle) #?. fighterPlacing fyr
        let a = RangeAttacking
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
      in case placing of
        Wide _ -> 
          let
            peacefulShifted = M.mapKeys (Hex.to E) peaceful
            peacefulUnion = M.unionWith (\_ x -> x) peaceful peacefulShifted
          in
            M peacefulUnion conflict
        Narrow _ -> M peaceful conflict
    Nothing -> M empty empty
