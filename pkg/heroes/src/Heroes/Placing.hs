module Heroes.Placing where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import qualified Heroes.Hex                                as Hex
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Maybe                                        (isJust)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Overlap
  = NoOverlap
  | OneOverlap Hex
  | TwoOverlaps Hex Hex
  deriving (Show)

--------------------------------------------------------------------------------

-- XXX don't export
wr :: Hex -> Hex
wr = Hex.to E

intersects :: Placing -> Placing -> Bool
intersects = isJust <<$>> intersectsAt1

intersectsAt1 :: Placing -> Placing -> Maybe Hex
intersectsAt1 = go
  where
  go (Narrow h1) (Narrow h2) = try h1 h2
  go (Narrow h1) (Wide h2)   = try h1 h2 <|> try h1 (wr h2)
  go (Wide h1) (Narrow h2)   = try h1 h2 <|> try h2 (wr h1)
  go (Wide h1) (Wide h2)     = try h1 h2 <|> try h2 (wr h1)
                                         <|> try h1 (wr h2)
  try a b = if a == b
            then Just a
            else Nothing

base :: Placing -> Hex
base (Narrow hex) = hex
base (Wide hex)   = hex

distance :: Placing -> Placing -> Int
distance (Narrow h0) p1 = distance' h0 p1
distance (Wide h0) p1 = min (distance' h0 p1) (distance' (wr h0) p1)

distance' :: Hex -> Placing -> Int
distance' h0 (Narrow h1) = Hex.distance h0 h1
distance' h0 (Wide h1) = min (Hex.distance h0 h1) (Hex.distance (wr h0) h1)

leftmost :: Placing -> Hex
leftmost (Narrow hex) = hex
leftmost (Wide hex) = hex

rightmost :: Placing -> Hex
rightmost (Narrow hex) = hex
rightmost (Wide hex) = wr hex

head :: Placing -> Facing -> Hex
head (Narrow hex) _ = hex
head (Wide hex) West = hex
head (Wide hex) East = wr hex

walk :: Bearing -> Placing -> Placing
walk bearing (Narrow hex) = Narrow (Hex.to bearing hex)
walk bearing (Wide hex)   = Wide (Hex.to bearing hex)

teleport :: Hex -> Placing -> Placing
teleport hex (Narrow _) = Narrow hex
teleport hex (Wide _)   = Wide hex

overlap :: Placing -> Placing -> Overlap
overlap = go
  where
  go (Narrow h1) (Narrow h2) = oh $ try h1 h2
  go (Narrow h1) (Wide h2)   = oh $ try h1 h2 <|> try h1 (wr h2)
  go (Wide h1) (Narrow h2)   = oh $ try h1 h2 <|> try h2 (wr h1)
  go (Wide h1) (Wide h2)     = if h1 == h2
                               then TwoOverlaps h1 (wr h1)
                               else oh $ try h2 (wr h1) <|> try h1 (wr h2)
  try a b = if a == b
            then Just a
            else Nothing
  -- this has a nice ring to it: "oh, try this! or try that!"
  oh Nothing = NoOverlap
  oh (Just hex) = OneOverlap hex

overlaps :: Placing -> Placing -> Bool
overlaps = fmap2 doesIt overlap
  where
  doesIt = \case
    NoOverlap -> False
    _ -> True

has :: Placing -> Hex -> Bool
has (Narrow h1) h = h1 == h
has (Wide h1) h   = h1 == h || wr h1 == h

isWide :: Placing -> Bool
isWide (Wide _) = True
isWide _ = False

collider :: Placing -> Hex -> Maybe Hex
collider (Narrow h1) h = if | Hex.adjacent h1 h -> Just h1
                            | otherwise -> Nothing
collider (Wide h1) h = if | Hex.adjacent h1 h -> Just h1
                          | Hex.adjacent h2 h -> Just h2
                          | otherwise -> Nothing
  where h2 = wr h1

visit :: Placing -> [Hex]
visit (Narrow hex) = [hex]
visit (Wide hex) = [hex, wr hex]

preferredFacing :: Placing -> Placing -> Maybe Facing
preferredFacing p0 p1 =
  case Hex.compareX (rightmost p0) (leftmost p1) of
    LT -> Just East
    _ -> case Hex.compareX (rightmost p1) (leftmost p0) of
      LT -> Just West
      _ -> Nothing
