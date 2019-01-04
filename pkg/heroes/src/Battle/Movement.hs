module Battle.Movement (
  movement,
  Movement(..)
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Battle
import Battle.Rules
import Battle.Setup
import Battle.Monad.Utils
import qualified Battle.PM                                 as PM
import qualified Heroes.Bearing                            as Bearing
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Monad.Writer                              (writer)
import qualified Data.Map.Strict                           as M
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Movement = M {
  peaceful :: M.Map Hex ([Move], Battle),
  conflict :: M.Map (Bearing, Hex) ([Move], Battle)
}

newtype Approaches = A (M.Map Hex [Move]) -- moves are in reverse order

data Situation = S Battle Hex Int [Move]

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
    Just (PM.Node hex pts, b) ->
      fst $
        (m0, A empty) & repeatedlyExplore [S b hex pts initialMoves]
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
  explore sit@(S b hex pts stack) (M p c, A a) =
    tryMoreMoves (M p' c', A a')
    where
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
        Just b' -> p & M.insert hex ((reverse (EOM : stack)), b')
        Nothing -> p
    --
    c' = c
    a' = a & M.insert hex stack
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
      Right (b', Last Nothing, PM.Node hex pts) ->
        let (wasThere, a') = a & insertMissing' hex stack'
        in ((M p c, A a'), if wasThere then [] else [S b' hex pts stack'])
      --
      Right (b', Last (Just (PM.Attack bearing hex)), _) ->
        let
          c' = c & insertMissing
            (Bearing.opposite bearing, hex)
            (reverse stack', b')
        in ((M p c', A a), [])
    where
    --
    stack' = m : stack
