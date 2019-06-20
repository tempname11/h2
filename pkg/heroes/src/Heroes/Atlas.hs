{-# LANGUAGE FlexibleContexts #-}
module Heroes.Atlas (
  Frame(..),
  Group,
  Blueprint(..),
  parse
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import qualified Heroes.Boxes                              as Boxes
import qualified Heroes.Griffin                            as Griffin
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.IntMap.Strict                        as I
import qualified Data.Vector                               as V
import qualified Data.Vector.Generic                       as GV
import qualified Data.Vector.Storable                      as SV
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Frame = Frame {
  box :: V2 CInt,
  place :: Point V2 CInt,
  offset :: V2 CInt
} deriving (Generic)

type Group = V.Vector Frame
type Poke m = V2 CInt -> ByteString -> m ()
type PokeCombinator m = Poke m -> m ()

data Blueprint m = Blueprint {
  groups :: V.Vector Group,
  pokeCombinator :: PokeCombinator m,
  dimensions :: V2 CInt,
  palette :: SV.Vector (V4 Word8)
}

--------------------------------------------------------------------------------

mkV2 :: (Num b, Integral a) => a -> a -> V2 b
mkV2 = V2 `on` (§)

parse :: forall m. Monad m => V2 CInt -> ByteString -> Either String (Blueprint m)
parse groundOffset buf = go <$> Griffin.parse buf
  where
  go :: Griffin.Data -> Blueprint m
  go data_ = Blueprint {
      groups = groups,
      pokeCombinator = pokeCombinator,
      dimensions = dimensions,
      palette = palette
    }
    where
    groups = f <$> Griffin.dGroups data_
    f :: Griffin.Group -> Group
    f group1 = GV.fromList . fmap f' . GV.toList $ Griffin.sOffsets group1
    f' ioffset = Frame { box, place, offset }
      where
      box = mkV2 w h
      offset = mkV2 ox oy - groundOffset
      place = P $ mkV2 px py
      w = Griffin.fWidth frame
      h = Griffin.fHeight frame
      ox = Griffin.fOffsetX frame
      oy = Griffin.fOffsetY frame
      frame = frameMap ! ioffset
      Boxes.Place px py = placeMap ! ioffset

    -- `places`, `keys`, `frames`, `boxes`
    --   all match their count & order.
    frameMap = Griffin.dFrames data_
    placeMap = I.fromList (zip keys places)
    places = snd fitResult
    keys   = I.keys frameMap
    frames = I.elems frameMap
    boxes = fmap toBox frames
    fitResult = Boxes.fitBest boxes
    toBox frame = (Boxes.Box `on` (§)) w h
      where
      w = Griffin.fWidth frame
      h = Griffin.fHeight frame
    --
    Boxes.Container cw ch = fst fitResult
    dimensions = mkV2 cw ch
    palette = Griffin.hPalette (Griffin.dHeader data_)
    --
    pokeCombinator poke = sequence_ actions
      where
      actions = zipWith f'' frames places
      f'' frame place = sequence_ subActions
        where
        ints = [0..] :: [Int]
        subActions = zipWith f''' rows ints
        rows = V.toList (Griffin.fRows frame)
        f''' row r = poke position row
          where
          position = corner + rowOffset
          Boxes.Place px py = place
          corner = mkV2 px py
          rowOffset = mkV2 0 r
