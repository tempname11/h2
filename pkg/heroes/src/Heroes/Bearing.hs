module Heroes.Bearing where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Heroes.Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Bimap                                as Bimap
import Data.Bimap                                        (Bimap)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Semi
  = Forward
  | Up
  | Down

deriving instance Eq Semi
deriving instance Ord Semi
deriving instance Show Semi

--------------------------------------------------------------------------------

closest :: Bearing -> [Bearing]
closest b0 = [b0, b1, b2, b3, b4, b5]
  where
  b1 = clockwise b0
  b2 = counterclockwise b0
  b3 = clockwise b1
  b4 = counterclockwise b2
  b5 = opposite b0

toFacing :: Bearing -> Facing
toFacing NW = West
toFacing SW = West
toFacing W = West
toFacing _ = East

bearingMap :: Bimap Bearing Bearing
bearingMap = Bimap.fromList
  [ ( W, NW)
  , (NW, NE)
  , (NE,  E)
  , ( E, SE)
  , (SE, SW)
  , (SW,  W) ]

list :: [Bearing]
list = [W, NW, NE, E, SE, SW]

clockwise :: Bearing -> Bearing
clockwise = (bearingMap Bimap.!)

counterclockwise :: Bearing -> Bearing
counterclockwise = (bearingMap Bimap.!>)

opposite :: Bearing -> Bearing
opposite NW = SE
opposite  W =  E
opposite SW = NE
opposite SE = NW
opposite  E =  W
opposite NE = SW

semi :: Bearing -> Semi
semi NW = Up
semi NE = Up
semi W = Forward
semi E = Forward
semi SW = Down
semi SE = Down
