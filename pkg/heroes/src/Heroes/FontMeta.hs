{-# LANGUAGE OverloadedLists #-}
module Heroes.FontMeta (
  FontMeta(..),
  GlyphMeta(..),
  get,
  put
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Binary.Get                                   (Get)
import Data.Binary.Get                                   (getWord16le)
import Data.Binary.Put                                   (Put)
import Data.Binary.Put                                   (putWord16le)
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data GlyphMeta = GlyphMeta {
  advanceX :: Int,
  place :: V2 Int,
  box :: V2 Int,
  renderOffset :: V2 Int
} deriving (Show, Generic)

data FontMeta = FontMeta {
  dimensions :: V2 Int,
  ascender :: Int,
  descender :: Int,
  glyphs :: V.Vector GlyphMeta
} deriving (Show, Generic)

put :: FontMeta -> Put
put (FontMeta {..}) = do
  put16 (dimensions ^. _x)
  put16 (dimensions ^. _y)
  put16 ascender
  put16 descender
  put16 (V.length glyphs)
  for_ glyphs $ \(GlyphMeta {..}) -> do
    put16 advanceX
    put16 (place ^. _x)
    put16 (place ^. _y)
    put16 (box ^. _x)
    put16 (box ^. _y)
    put16 (renderOffset ^. _x)
    put16 (renderOffset ^. _y)

-- XXX from SpriteMeta
magic :: Integral a => a
magic = 32768

-- XXX from SpriteMeta
put16 :: Integral a => a -> Put
put16 n = do
  let n' = n + magic
  when (n' < 0 || n' > 65535) $ fail "SpriteMeta: number out of range."
  let n'' = (§) n'
  putWord16le n''

-- XXX from SpriteMeta
get16 :: Integral a => Get a
get16 = (subtract magic) . (§) <$> getWord16le

v2 :: Get a -> Get (V2 a)
v2 g = V2 <$> g <*> g

get :: Get FontMeta
get = do
  dimensions <- v2 get16
  ascender <- get16
  descender <- get16
  gLength <- get16
  gs <- replicateM gLength $ do
    advanceX <- get16
    place <- v2 get16
    box <- v2 get16
    renderOffset <- v2 get16
    return (GlyphMeta {..})
  --
  return $
    FontMeta {
      dimensions,
      ascender,
      descender,
      glyphs = V.fromList gs
    }
    
