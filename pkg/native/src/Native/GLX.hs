{-# OPTIONS_GHC -Wno-orphans #-}
module Native.GLX where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.GLX
import Native
import Native.GLES ()
import Native.WND'SDL ()
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Foreign.C.String                                  (peekCString)
import Foreign.ForeignPtr                                (newForeignPtr_)
import Foreign.Marshal.Alloc                             (mallocBytes)
import Foreign.Ptr                                       (castPtr)
import Foreign.Ptr                                       (plusPtr)
import Foreign.Storable                                  (poke)
import SDL                                               (($=))
import System.IO                                         (readFile)
import qualified Data.Vector.Storable                      as SV
import qualified Graphics.GL.Core32                        as GL
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance GLX where
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
    -- return ((§) size, fPtr)
    undefined fPtr
  --
  loadGLSL = readFile
  --
  getGLContext window = do
    void $ SDL.glCreateContext window
    SDL.swapInterval $= SDL.SynchronizedUpdates
    words <$> (GL.glGetString GL.GL_EXTENSIONS >>= peekCString . castPtr)
  --
  generatePaletteArray palette = do
    let
      channel (V4 r g b a) = \case
        0 -> r
        1 -> g
        2 -> b
        _ -> a
      --
      v = SV.generate 1024 $ \i' ->
        let (i, c) = i' `divMod` 4
        in if i > 8
          then channel (palette ! i) c
          else 0
    --
    let (ptr, size) = SV.unsafeToForeignPtr0 v
    return ((§) size, ptr)
