module Native.Types (
  Chunk,
  ComplexSprite,
  StaticSprite
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Native
import qualified Native.ComplexSprite
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified SDL.Mixer                                 as Mix
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Chunk = Mix.Chunk
type ComplexSprite = Native.ComplexSprite.ComplexSprite
type StaticSprite = Native.StaticSprite
