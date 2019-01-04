module Heroes.H3.Misc where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Heroes.Internal                                   (HexDiff(..))
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Set                                  as S
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

-- XXX rename this file!
--
data ObstacleType
  = Obstacle'0
  deriving (Eq, Ord, Enum, Bounded, Show)

oImgName :: ObstacleType -> String
oImgName = \case
  Obstacle'0 -> "ObCFL00"

obstacleDiffs :: ObstacleType -> Set HexDiff
obstacleDiffs Obstacle'0 =
  S.fromList [
    (HexDiff 1 1),
    (HexDiff 2 2),
    (HexDiff (-1) 0),
    (HexDiff 0 1),
    (HexDiff 1 2),
    (HexDiff 2 3),
    (HexDiff (-1) 1),
    (HexDiff 0 2),
    (HexDiff 1 3)
  ]

obstacleOffset :: ObstacleType -> V2 CInt
obstacleOffset Obstacle'0 = V2 (-40) 2
