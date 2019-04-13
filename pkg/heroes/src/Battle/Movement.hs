module Battle.Movement (
  movement,
  Movement(..),
  MovementResult(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Battle
import Battle.Rules
import Battle.Setup
import Battle.Monad.Utils
import qualified Battle.PM                                 as PM
import qualified Heroes.Bearing                            as Bearing
import qualified Heroes.Placing                            as Placing
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Monad.Writer                              (writer)
import qualified Data.Map.Strict                           as M
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data MovementResult = MR {
  pointsLeft :: Int,
  moves :: [Move], 
  destinationPlacing :: Placing,
  battle :: Battle
} deriving (Generic)

data Movement = M {
  peaceful :: M.Map Hex MovementResult,
  conflict :: M.Map (Bearing, Hex) MovementResult
} deriving (Generic)

newtype Approaches = A (M.Map Hex [Move]) -- moves are in reverse order

data Situation = S Battle Placing Int [Move]

--------------------------------------------------------------------------------

m0 :: Movement
m0 = M empty empty

insertMissing :: Ord k => k -> a -> M.Map k a -> M.Map k a
insertMissing = M.insertWith (\_ a -> a)

insertMissing' :: Ord k => k -> a -> M.Map k a -> (Bool, M.Map k a)
insertMissing' k x m =
  over _1 justIsTrue $
    M.insertLookupWithKey (\_ _ a -> a) k x m

movement :: [Move] -> (Setup, Battle) -> Movement
movement initialMoves (s, b0) =
  case (s, b0) #?.! do
    for_ initialMoves makeMove
    pNode
  of
    Just (PM.Node { currentPlacing,  points }, b) ->
      fst $
        (m0, A empty) & repeatedlyExplore [S b currentPlacing points initialMoves]
    Nothing -> m0
  where
  --
  repeatedlyExplore ::
    [Situation] ->
    (Movement, Approaches) ->
    (Movement, Approaches)
  repeatedlyExplore ss ma =
    let
      (ma', ss') = runWriter $ (foldl (>=>) return $ explore <$> ss) ma
    in case ss' of
      [] -> ma'
      _ -> repeatedlyExplore ss' ma'
  --
  explore ::
    Situation ->
    (Movement, Approaches) ->
    Writer [Situation] (Movement, Approaches)
  --
  explore sit@(S b currentPlacing pts stack) (M p c, A a) =
    tryMoreMoves (M p' c', A a')
    where
    --
    base = Placing.base currentPlacing
    --
    tryMoreMoves =
      if pts >= 0
        then tryAllBearings
        else return
    --
    tryAllBearings =
      foldl (>=>) return $
        (try sit . BearingSelected) <$> Bearing.list
    --
    afterEom = (s, b) #?! makeMove EOM
    --
    p' =
      case afterEom of
        Just b' ->
          let
            r = MR {
              pointsLeft = pts,
              moves = reverse (EOM : stack),
              battle = b',
              destinationPlacing = currentPlacing
            }
          in p & M.insert base r
        Nothing -> p
    --
    c' = c
    a' = a & M.insert base stack
    --
  try ::
    Situation ->
    Move -> 
    (Movement, Approaches) ->
    Writer [Situation] (Movement, Approaches)
  --
  try (S b _ _ stack) m (M p c, A a) = writer $
    case (s, b) #%!*. (makeMove m >> pNode) of
      Left _ -> ((M p c, A a), [])
      --
      Right (b', Last Nothing, PM.Node { currentPlacing, points }) ->
        let
          base = Placing.base currentPlacing
          (wasThere, a') = a & insertMissing' base stack'
        in ((M p c, A a'), if wasThere then [] else [S b' currentPlacing points stack'])
      --
      Right (b', Last (Just (PM.Attack { attackerPlacing, bearing, hit })), _) ->
        let afterEom = (s, b') #?! makeMove EOM
        in case afterEom of
          Nothing -> ((M p c, A a), [])
          Just b'' ->
            let
              c' = c & insertMissing
                (Bearing.opposite bearing, hit)
                MR {
                  pointsLeft = 0,
                  moves = reverse (EOM : stack'),
                  battle = b'',
                  destinationPlacing = attackerPlacing
                }
            in ((M p c', A a), [])
    where
    --
    stack' = m : stack
