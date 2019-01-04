module Battle.AI.Primitive (
  ai
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Battle
import Battle.Rules
import Battle.Setup
import Battle.Monad.Utils
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Maybe                                        (isJust)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

ai :: (Setup, Battle) -> Maybe [Move]
ai (s, b) = case filter isGood $ acceptableMoves (s, b) of
  i : _ -> Just [i]
  [] -> Nothing
  where
  isGood m = isJust ((s, b) #?. makeMove m)
