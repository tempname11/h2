{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Native.GLES ()
import Native.Utils                                      (createPalettedSurface)
import Heroes.Platform
import qualified Heroes.Atlas                              as Atlas
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (forkOS)
import Foreign.ForeignPtr                                (newForeignPtr_)
import Foreign.Marshal.Alloc                             (mallocBytes)
import Foreign.Ptr                                       (castPtr)
import Foreign.Ptr                                       (plusPtr)
import Foreign.Storable                                  (poke)
import System.IO                                         (readFile)
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
  forkPreferred = forkOS
  --
  type StaticSprite = NativeStaticSprite
  type CursorResources = V.Vector (V.Vector SDL.Cursor)
  type Renderer = SDL.Renderer
  type InputProvider = ()
  --
  type ComplexSprite = NativeComplexSprite
  loadComplexSprite _ meta pngPath = do
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
  createQuadArray = do
    let size = 12 * (4 {- float32 -})
    ptr <- mallocBytes size
    poke (castPtr $ plusPtr ptr 0) (1 :: Float)
    poke (castPtr $ plusPtr ptr 4) (1 :: Float)
    poke (castPtr $ plusPtr ptr 8) (0 :: Float)
    poke (castPtr $ plusPtr ptr 12) (1 :: Float)
    poke (castPtr $ plusPtr ptr 16) (0 :: Float)
    poke (castPtr $ plusPtr ptr 20) (0 :: Float)
    poke (castPtr $ plusPtr ptr 24) (0 :: Float)
    poke (castPtr $ plusPtr ptr 28) (0 :: Float)
    poke (castPtr $ plusPtr ptr 32) (1 :: Float)
    poke (castPtr $ plusPtr ptr 36) (0 :: Float)
    poke (castPtr $ plusPtr ptr 40) (1 :: Float)
    poke (castPtr $ plusPtr ptr 44) (1 :: Float)
    fPtr <- newForeignPtr_ $ castPtr ptr
    return ((§) size, fPtr)
  loadGLString = readFile

data NativeStaticSprite = NativeStaticSprite {
  texture    :: SDL.Texture,
  dimensions :: V2 CInt
}

data NativeComplexSprite = NativeComplexSprite {
  surface :: SDL.Surface,
  palette :: SDL.Palette,
  groups  :: V.Vector Atlas.Group
}

instance Show NativeComplexSprite where
  show _ = "NativeComplexSprite" -- XXX

makeShorthands ''NativeStaticSprite
makeShorthands ''NativeComplexSprite
