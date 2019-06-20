module Heroes.Boxes where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
import qualified Data.IntMap.Strict                        as IntMap
import qualified Data.IntSet                               as IntSet
import Data.List                                         (head) -- XXX partial
import Data.List                                         (span)
import Data.List                                         (break)
import Data.List                                         (dropWhile)
import Data.List                                         (minimumBy) -- XXX partial
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Box = Box Int Int
data Container = Container Int Int
data Place = Place Int Int

data Crease = Crease Int Int
type Queue = [Index]

newtype Score = Score Int
type Index = Int
type Tuple = (Index, Crease, Score)

deriving instance Show Box
deriving instance Show Container
deriving instance Show Place
deriving instance Show Crease
deriving instance Show Score
deriving instance Eq Score

--------------------------------------------------------------------------------

cmpW  :: Box -> Box -> Ordering
cmpH  :: Box -> Box -> Ordering
cmpWi :: Box -> Box -> Ordering
cmpHi :: Box -> Box -> Ordering

cmpW  (Box w1 _) (Box w2 _) = compare w1 w2
cmpH  (Box _ h1) (Box _ h2) = compare h1 h2
cmpWi (Box w1 _) (Box w2 _) = compare w2 w1
cmpHi (Box _ h1) (Box _ h2) = compare h2 h1

fit1 :: Container -> [Box] -> Maybe [Place]
fit1 container boxes_ = fromMap <$> recursion iindices iqueues icreases where

  boxes :: V.Vector Box
  boxes = V.fromList boxes_

  iindices_ :: [Int]
  iindices_ = [0..V.length boxes - 1]

  iindices :: IntSet
  iindices = IntSet.fromDistinctAscList iindices_

  icreases :: [Crease]
  icreases = [Crease 0 0]

  adapt :: (Box -> Box -> Ordering) -> Index -> Index -> Ordering
  adapt cmp = cmp `on` (V.!) boxes

  iqueues :: [Queue]
  iqueues = fmap ((`sortBy` iindices_) . adapt) [cmpWi, cmpHi, cmpW, cmpH]

  fromMap :: IntMap Place -> [Place]
  fromMap = fmap snd . IntMap.toAscList

  recursion :: IntSet -> [Queue] -> [Crease] -> Maybe (IntMap Place)
  recursion indices queues creases
    | IntSet.null indices = Just IntMap.empty
    | bestScore == Score maxBound = Nothing
    | otherwise = (IntMap.insert bestIndex bestPlace) <$>
                  recursion indices' queues' creases'
      where
        indices' :: IntSet
        indices' = IntSet.delete bestIndex indices

        queues' :: [Queue]
        queues' = fmap (dropWhile (`IntSet.notMember` indices')) queues

        creases' :: [Crease]
        creases' = p1 <> p2' <> p3
            where (p1, rest) = span notObstructed creases
                  (_, p3)  = break notObstructed rest
                  p2' = [Crease bx0 by1, Crease bx1 by0]

        notObstructed :: Crease -> Bool
        notObstructed (Crease x y) = (x < bx0 || x > bx1) &&
                                     (y < by0 || y > by1)

        Place bx0 by0 = bestPlace
        Box bw bh = bestBox
        bx1 = bx0 + bw
        by1 = by0 + bh

        bestBox :: Box
        bestBox = boxes V.! bestIndex

        bestPlace :: Place
        bestPlace = (\(Crease x y) -> Place x y) bestCrease

        bestIndex :: Index
        bestCrease :: Crease
        bestScore :: Score
        (bestIndex, bestCrease, bestScore) = minimumBy cmpScore tuples

        cmpScore :: Tuple -> Tuple -> Ordering
        cmpScore (_,_,Score s1) (_,_,Score s2) = compare s1 s2

        tuples :: [Tuple]
        tuples = [(a, b, scoreOf a b) | a <- candidates, b <- creases]

        scoreOf :: Index -> Crease -> Score
        scoreOf index crease = Score $ if insideContainer
                                       then sumLeft + sumTop
                                       else maxBound
          where
          insideContainer = x1 <= cw && y1 <= ch
          Container cw ch = container
          Box w h = boxes V.! index
          Crease x0 y0 = crease
          x1 = x0 + w
          y1 = y0 + h

          leftCreases  :: [Crease]
          leftCreases' :: [Crease]
          topCreases   :: [Crease]
          topCreases'  :: [Crease]

          (leftCreases, _:topCreases) = span isLeft creases
          isLeft (Crease x _) = x < x0

          leftCreases' = filter aboveY1 leftCreases
          topCreases' = reverse (filter leftX1 topCreases)

          aboveY1 (Crease _ y) = y < y1
          leftX1 (Crease x _) = x < x1

          sumLeft = fst $ foldl stepLeft (0, (x0, y1)) leftCreases'
          sumTop = fst $ foldl stepTop (0, (x1, y0)) topCreases'

          stepLeft (s, (px, py)) (Crease x y) = (s', (px, y))
            where s' = s + (px - x) * (py - y)
          stepTop (s, (px, py)) (Crease x y) = (s', (x, py))
            where s' = s + (px - x) * (py - y)

        candidates :: [Index]
        candidates = fmap head queues

fitBest :: [Box] -> (Container, [Place])
fitBest boxes = recursion baseContainer
  where
  baseContainer = Container (2 ^ (a + b)) (2 ^ a)
  a :: Int
  b :: Int
  (a, b) = (`divMod` 2) $ ceiling (log totalArea / log 2)
  totalArea :: Float
  totalArea = (§) $ sum [w * h | Box w h <- boxes]
  recursion c = case fit1 c boxes of
                  Nothing -> recursion c'
                  Just places -> (c, places)
                where
                Container w h = c
                c' = if w > h
                     then Container w (2 * h)
                     else Container (2 * w) h
