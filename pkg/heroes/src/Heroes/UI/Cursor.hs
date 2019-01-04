module Heroes.UI.Cursor (
  parse,
  number,
  Group (..),
  Pointing (..),
  Blueprint (..),
  Poke,
  PokeCombinator,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import qualified Heroes.Griffin                            as Griffin
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
import qualified Data.Vector.Storable                      as SV
import qualified Data.IntMap.Strict                        as IntMap
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Poke m = Int -> V2 CInt -> ByteString -> m ()
type PokeCombinator m = Poke m -> m ()

data Blueprint m = Blueprint
  { bPokeCombinator :: !( PokeCombinator m     )
  , bDimensions     :: !( V2 CInt              )
  , bCount          :: !( Int                  )
  , bPalette        :: !( SV.Vector (V4 Word8) )
  }

data Pointing = Up | Down | To Bearing

data Group
  = Normal
  | Question
  | Blocked
  | Run
  | Fly
  | Sword Pointing
  | Arrow
  | BrokenArrow
  | Helmet
  | Heal
  | Sacrifice
  | Portal
  | Catapult
  | Hourglass
  | Spellbook

--------------------------------------------------------------------------------

number :: Group -> Int
number = go
  where
  go Normal          = 0
  go Question        = 1
  go Blocked         = 2
  go Run             = 3
  go Fly             = 4
  go (Sword Up)      = 5
  go (Sword (To NE)) = 6
  go (Sword (To E))  = 7
  go (Sword (To SE)) = 8
  go (Sword Down)    = 9
  go (Sword (To SW)) = 10
  go (Sword (To W))  = 11
  go (Sword (To NW)) = 12
  go Arrow           = 13
  go BrokenArrow     = 14
  go Helmet          = 15
  go Heal            = 16
  go Sacrifice       = 17
  go Portal          = 18
  go Catapult        = 19
  go Hourglass       = 22
  go Spellbook       = 23

parse :: forall m. Monad m => ByteString -> Either String (Blueprint m)
parse buf = go <$> Griffin.parse buf
  where
  go data_ = Blueprint pokeCombinator dimensions count palette
    where
    pokeCombinator poke = sequence_ actions
      where
      actions = zipWith f'' (IntMap.elems frames) ints
      ints = [0..] :: [Int]
      f'' frame i = sequence_ subActions
        where
        subActions = zipWith f''' rows ints
        rows = V.toList (Griffin.fRows frame)
        x = (§) $ Griffin.fOffsetX frame
        y = (§) $ Griffin.fOffsetY frame
        f''' row r = poke i position row
          where
          position :: V2 CInt
          position = V2 x (y + (§) r)
    dimensions = (<§>) (V2 w h)
      where
      w = Griffin.hWidth header
      h = Griffin.hHeight header
    count = IntMap.size frames
    palette = Griffin.hPalette header
    frames = Griffin.dFrames data_
    header = Griffin.dHeader data_
