module Heroes.SpriteMeta (
  Palette,
  SpriteMeta(..),
  get,
  put,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import qualified Data.Vector                               as V
import qualified Data.Vector.Storable                      as SV
import qualified Heroes.Atlas                              as Atlas
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Binary.Get                                   (Get)
import Data.Binary.Get                                   (getWord16le)
import Data.Binary.Get                                   (getWord8)
import Data.Binary.Put                                   (Put)
import Data.Binary.Put                                   (putWord16le)
import Data.Binary.Put                                   (putWord8)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Palette = SV.Vector (V4 Word8)

data SpriteMeta = SpriteMeta {
  dimensions :: V2 CInt,
  palette    :: Palette,
  groups     :: V.Vector Atlas.Group
} deriving (Generic)

magic :: Integral a => a
magic = 32768

--------------------------------------------------------------------------------

put :: SpriteMeta -> Put
put meta = do
  put16 w
  put16 h
  put16 $ SV.length palette

  SV.forM_ palette $ \(V4 r g b a) -> do
    putWord8 r
    putWord8 g
    putWord8 b
    putWord8 a

  put16 $ V.length groups
  for_ groups $ \group -> do
    put16 $ V.length group
    for_ group $ \frame -> do
     v2 put16 $ frame ^. #box
     let P place = frame ^. #place
     v2 put16 place
     v2 put16 $ frame ^. #offset

  where
  (V2 w h) = meta ^. #dimensions
  palette  = meta ^. #palette
  groups   = meta ^. #groups
  v2 :: (a -> Put) -> V2 a -> Put
  v2 p (V2 x y) = p x >> p y

put16 :: Integral a => a -> Put
put16 n = do
  let n' = n + magic
  when (n' < 0 || n' > 65535) $ fail "SpriteMeta: number out of range."
  let n'' = (§) n'
  putWord16le n''

--------------------------------------------------------------------------------

get16 :: Integral a => Get a
get16 = (subtract magic) . (§) <$> getWord16le

get :: Get SpriteMeta
get = do
  dimensions <- v2 get16

  pLength <- get16
  palette <- SV.replicateM pLength $ do
    r <- getWord8
    g <- getWord8
    b <- getWord8
    a <- getWord8
    return $ V4 r g b a

  gsLength <- get16
  groups <- V.replicateM gsLength $ do
    gLength <- get16
    V.replicateM gLength $ do
      box    <- v2 get16
      place  <- v2 get16
      offset <- v2 get16
      return $ Atlas.Frame {
        Atlas.box    = box,
        Atlas.place  = P place,
        Atlas.offset = offset
      }

  return $ SpriteMeta {
    dimensions = dimensions,
    palette = palette,
    groups = groups
  }

  where
  v2 :: Get a -> Get (V2 a)
  v2 g = V2 <$> g <*> g
