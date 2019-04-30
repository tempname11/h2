module Battle.AI.Search (
  ai
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Battle
import Battle.Rules
import Battle.Setup
import Battle.Monad
import Battle.Movement
import Battle.Monad.Utils
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Fix                                          (Fix, cata, ana)
import Data.List                                         (maximumBy) -- XXX partial
import Control.Monad.State                               (get)
import qualified Data.Map.Strict                           as M
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Branching a = Branching Battle [([Move], a)]
  deriving Functor

-- XXX Acceptable, Unacceptable?
data Result = Result {
  prediction :: Battle,
  bestMoves :: Maybe [Move]
} deriving (Generic)

-- XXX make it a parameter
numberOfEoms :: Int
numberOfEoms = 2

heuristic :: Team -> Setup -> Battle -> Int
heuristic t s b =
  case (s, b) #?. heuristic' t of
    Just i -> i
    Nothing -> minBound

heuristic' :: Team -> P Int
heuristic' t = do
  fighters <- (?) _fighters
  let
    (ours, theirs) =
      partition (\FighterAttr { team } -> team == t) (M.elems fighters)
  return (length ours - length theirs)

cutoff :: [Move] -> [Move]
cutoff (EOM : _) = EOM : []
cutoff (m : ms) = m : cutoff ms
cutoff [] = []

ai :: (Setup, Battle) -> Maybe [Move]
ai (setup, battle) =
  fmap cutoff .
  view _bestMoves .
  deepResult setup .
  deepBranch setup $
    (battle, numberOfEoms)

deepResult :: Setup -> Fix Branching -> Result
deepResult setup = cata (result setup)

result :: Setup -> Branching Result -> Result
result s (Branching b bs) = case bs of
  [] -> Result {
    prediction = b,
    bestMoves = Nothing
  }
  _ -> convert (maximumBy (comparing (heu . prediction . snd)) bs)
  where
  heu = heuristic team s
  team = (fst . currentS) (b ^. _order)
  convert (moves, Result { bestMoves = futureMoves, prediction = p }) =
    Result {
      prediction = p,
      bestMoves = Just (moves <> maybe [] id futureMoves)
    }

deepBranch :: Setup -> (Battle, Int) -> Fix Branching
deepBranch setup = ana (branch setup)

assumingOneEOM :: Int
assumingOneEOM = 1

branch :: Setup -> (Battle, Int) -> Branching (Battle, Int)
branch s (b, eomsLeft) =
  if eomsLeft <= 0
  then Branching b []
  else case b ^. _phase of
    Phase'Movement {} ->
      Branching b $
        let M p c = movement [] (s, b)
        in
          (M.elems p <> M.elems c) <&>
            \(MR { moves, battle = b' }) -> (moves, (b', eomsLeft - assumingOneEOM))
    --
    _ -> Branching b $ mapMaybe id (apply <$> acceptableMoves' (s, b))
  --
  where
  apply m = (s, b) #?. do
    makeMove m
    b' <- get
    let eomCount = case m of { EOM -> 1; _ -> 0 }
    return ([m], (b', eomsLeft - eomCount))
