module Heroes.Cell where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Internal
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

halfSize :: V2 CInt
halfSize = V2 22 21

fromHex :: Hex -> V2 CInt
fromHex (Hex d q) = fromHex' d q - halfSize

fromHex' :: Int -> Int -> V2 CInt
fromHex' d q = halfSize * V2 n m
  where
  n =        (§) $ q + d
  m = (*2) . (§) $ q - d

toSegment :: V2 CInt -> Segment
toSegment v = Segment hex bearing
  where
  -- this code is pretty hard to read...
  hex = Hex (d + di) (q + qi)
  n, m :: Float
  V2 n m = (<§>) v / (<§>) halfSize / 2
  q = floor $ n + m / 2
  d = floor $ n - m / 2
  V2 x y = v - fromHex' d q
  b = x - 2 * y
  p = x + 2 * y
  (di, qi) = if x < 24 -- constants in this section chosen by hand
                then if | y >= 0 && p >= 70 -> (0, 1)
                        | y <  0 && b >= 40 -> (1, 0)
                        | otherwise         -> (0, 0)
                else if | y >= 0 && b < -23 -> (0, 1)
                        | y <  0 && p <  10 -> (1, 0)
                        | otherwise         -> (1, 1)

  bearing = case atan2 xx yy / pi * 180 of
              a | a >= 120 -> NE
                | a >= 60 -> E
                | a >= 0 -> SE
                | a >= -60 -> SW
                | a >= -120 -> W
                | otherwise -> NW
  xx, yy :: Float
  V2 xx yy = (<§>) $ v - fromHex' (d + di) (q + qi)
