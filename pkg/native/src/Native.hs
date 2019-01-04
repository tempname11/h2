{-# LANGUAGE TemplateHaskell #-}
module Native (
  module Heroes,
  module Common.IO,
  module Common.LowLevel,
  StaticSprite(..),
  Stamp(..),
  CopyCommand(..),
  DrawingAct(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Common.IO
import Common.LowLevel
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data StaticSprite = StaticSprite {
  texture    :: SDL.Texture,
  dimensions :: V2 CInt
}

data Stamp = Stamp {
  surface :: SDL.Surface, -- XXX suspicious: might only need 1 of these ever
  texture :: SDL.Texture
}

data CopyCommand = CopyCommand {
  texture :: SDL.Texture,
  src     :: Maybe (SDL.Rectangle CInt),
  dst     :: Maybe (SDL.Rectangle CInt),
  flips   :: V2 Bool
}

data DrawingAct = DrawingAct {
  curtain :: Word8,
  outline :: [Hex],
  shaded  :: [Hex],
  copies  :: [CopyCommand]
}

--------------------------------------------------------------------------------

makeShorthands ''Stamp
makeShorthands ''StaticSprite
makeShorthands ''CopyCommand
makeShorthands ''DrawingAct
