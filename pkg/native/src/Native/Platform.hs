{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Heroes.Platform
import qualified Heroes.Atlas                              as Atlas
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (forkOS)
import qualified Data.Vector                               as V
import qualified SDL
import qualified SDL.Mixer                                 as Mix
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Platform where
  productionPrefix = ".production-assets/"
  staticSpriteExtension = ".bmp"
  type StaticSprite = NativeStaticSprite
  type ComplexSprite = NativeComplexSprite
  type Chunk = Mix.Chunk
  forkPreferred = forkOS

data NativeStaticSprite = NativeStaticSprite {
  texture    :: SDL.Texture,
  dimensions :: V2 CInt
}

data NativeComplexSprite = NativeComplexSprite {
  surface :: SDL.Surface,
  palette :: SDL.Palette,
  groups  :: V.Vector Atlas.Group
}

makeShorthands ''NativeStaticSprite
makeShorthands ''NativeComplexSprite
