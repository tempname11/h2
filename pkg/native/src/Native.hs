module Native (
  CopyCommand(..),
  DrawingAct(..),
  Platform,
  Stamp(..),
  module Heroes,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Platform                                   (Platform)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Stamp = Stamp {
  surface :: SDL.Surface, -- XXX suspicious: might only need 1 of these ever
  texture :: SDL.Texture
} deriving (Generic)

data CopyCommand = CopyCommand {
  texture :: SDL.Texture,
  src     :: Maybe (SDL.Rectangle CInt),
  dst     :: Maybe (SDL.Rectangle CInt),
  flips   :: V2 Bool
} deriving (Generic)

data DrawingAct = DrawingAct {
  curtain :: Word8,
  outline :: [Hex],
  shaded  :: [Hex],
  copies  :: [CopyCommand]
} deriving (Generic)
