{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Platform
import Native
import Native.Image ()
import Native.GLES'Types ()
import Native.GLES'Utils                                 (makeContext)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (forkOS)
import Foreign.ForeignPtr                                (newForeignPtr_)
import Foreign.Marshal.Alloc                             (mallocBytes)
import Foreign.Ptr                                       (castPtr)
import Foreign.Ptr                                       (plusPtr)
import Foreign.Storable                                  (poke)
import System.IO                                         (readFile)
import qualified Codec.Picture                             as Juicy
import qualified SDL.Mixer                                 as Mix
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Platform where
  --
  productionPrefix = ".production-assets/"
  staticSpriteExtension = ".bmp"
  forkPreferred = forkOS
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
  getGLContext _ = makeContext
  loadImage = Juicy.readPng
  --
  generatePaletteArray palette = do
    undefined palette
