{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Native.Utils                                      (createPalettedSurface)
import Heroes.Platform
import qualified Heroes.Atlas                              as Atlas
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (forkOS)
import qualified Codec.Picture                             as Juicy
import qualified Data.Vector                               as V
import qualified Data.Vector.Storable                      as SV
import qualified SDL
import qualified SDL.Mixer                                 as Mix
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Platform where
  --
  productionPrefix = ".production-assets/"
  staticSpriteExtension = ".bmp"
  --
  type StaticSprite = NativeStaticSprite
  type CursorResources = V.Vector (V.Vector SDL.Cursor)
  type Renderer = SDL.Renderer
  --
  type ComplexSprite = NativeComplexSprite
  loadComplexSprite meta pngPath = do
    putStrLn $ "Loading... " <> pngPath
    result <- Juicy.readPng pngPath
    --
    Juicy.Image _ _ pixels <- case result of
      Left str -> raise str
      Right (Juicy.ImageY8 i) -> return i
      _ -> raise "Juicy image format mismatch."
    --
    mpixels <- SV.unsafeThaw pixels
    --
    (surface, palette) <- createPalettedSurface mpixels
      (meta ^. palette_) (meta ^. dimensions_)
    --
    return $ NativeComplexSprite {
      surface = surface,
      palette = palette,
      groups  = meta ^. groups_
    }
  destroyComplexSprite s = SDL.freeSurface (s ^. surface_)
  --
  type Chunk = Mix.Chunk
  loadChunk = Mix.load
  freeChunk = Mix.free
  --
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
