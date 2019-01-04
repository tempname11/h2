module Heroes.Hex where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Internal
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Set                                  as S
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

fromMulti :: Multiplacing -> Set Hex
fromMulti (Multiplacing base diffs) = (base `addHexDiff`) `S.map` diffs

addHexDiff :: Hex -> HexDiff -> Hex
addHexDiff (Hex d q) (HexDiff dd qq) = Hex (d + dd) (q + qq)

neighbors :: Hex -> [Hex]
neighbors x = fmap (flip to x) bearings
  where bearings = [E, SE, SW, W, NW, NE]

adjacent :: Hex -> Hex -> Bool
adjacent (Hex d1 q1) (Hex d2 q2) =
  q1 == q2 && abs (d1 - d2) == 1 ||
  d1 == d2 && abs (q1 - q2) == 1 ||
  i1 == i2 && abs (o1 - o2) == 2
  where
  o1 = d1 + q1
  o2 = d2 + q2
  i1 = d1 - q1
  i2 = d2 - q2

distance :: Hex -> Hex -> Int
distance (Hex d1 q1) (Hex d2 q2) =
  if
    | d > 0 && q > 0 -> d + q - (min d q)
    | d < 0 && q < 0 -> (max d q) - d - q
    | otherwise -> abs d + abs q
  where
  d = (d1 - d2)
  q = (q1 - q2)


compareX :: Hex -> Hex -> Ordering
(Hex d1 q1) `compareX` (Hex d2 q2) = (d1 + q1) `compare` (d2 + q2)

standardField :: Int -> Int -> Set Hex
standardField halfW halfH = S.fromList $ do
  q <- [-s .. s]
  d <- [minD q..maxD q]
  return $ Hex d q
  where
    minD q = max (q - h) (negate (2 * w) - q)
    maxD q = min (q + h) ( (1 +) (2 * w) - q)
    w = halfW
    h = halfH
    s = w + h

to :: Bearing -> Hex -> Hex
to E  (Hex d q) = Hex (d+1) (q+1)
to W  (Hex d q) = Hex (d-1) (q-1)
to NE (Hex d q) = Hex (d+1) q
to NW (Hex d q) = Hex d (q-1)
to SE (Hex d q) = Hex d (q+1)
to SW (Hex d q) = Hex (d-1) q

