import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)

import Boxes

import Test.QuickCheck                  (Arbitrary)
import Test.QuickCheck                  (arbitrary)
import Test.QuickCheck.Modifiers        (Positive (Positive))

--------------------------------------------------------------------------------

main = defaultMain tests

tests = [ testGroup "All tests"
            [ testProperty "ok" prop_ok
            ]
        ]

intersect :: (Box, Place) -> (Box, Place) -> Bool
intersect (Box w1 h1, Place x1 y1) (Box w2 h2, Place x2 y2)
    | x1 >= (x2 + w2) = False
    | x2 >= (x1 + w1) = False
    | y1 >= (y2 + h2) = False
    | y2 >= (y1 + h1) = False
    | otherwise       = True

intersectAny :: (Box, Place) -> [(Box, Place)] -> Bool
intersectAny _ [] = False
intersectAny x0 (x1:xs) = intersect x0 x1 || intersectAny x0 xs
    -- intersectAny = any . intersect

intersectAny2 :: [(Box, Place)] -> Bool
intersectAny2 [] = False
intersectAny2 (x:xs) = intersectAny x xs || intersectAny2 xs

inBounds :: Container -> (Box, Place) -> Bool
inBounds (Container x1 y1) (Box w h, Place x0 y0) = x0 + w <= x1 && y0 + h <= y1

inBoundsAll :: Container -> [(Box, Place)] -> Bool
inBoundsAll c xs = all (inBounds c) xs

instance Arbitrary Box where
    arbitrary = do
        Positive w <- arbitrary
        Positive h <- arbitrary
        return $ Box w h

prop_ok :: [Box] -> Bool
prop_ok boxes = inBoundsAll container pairs && not (intersectAny2 pairs)
    where
        (container, places) = fitBest boxes
        pairs = zip boxes places
