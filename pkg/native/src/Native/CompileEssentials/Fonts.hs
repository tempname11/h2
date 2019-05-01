module Native.CompileEssentials.Fonts where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Boxes                                      (Box(..))
import Heroes.Boxes                                      (Container(..))
import Heroes.Boxes                                      (Place(..))
import Heroes.Boxes                                      (fitBest)
import Heroes.Font                                       (charSet)
import Heroes.FontMeta                                   (FontMeta(..))
import Heroes.FontMeta                                   (GlyphMeta(..))
import Native
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Bits                                         (shiftL)
import Foreign.C.String                                  (withCString)
import Foreign.C.Types                                   (CUInt)
import Foreign.Marshal.Alloc                             (alloca)
import Foreign.Ptr                                       (plusPtr)
import Foreign.Storable                                  (peek)
import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Prelude                                           (fromEnum)
import qualified Codec.Picture                             as Juicy
import qualified Data.Map.Strict                           as M
import qualified Data.Vector.Storable                      as SV
import qualified Data.Vector.Storable.Mutable              as MSV
import qualified Graphics.Rendering.FreeType.Internal.Bitmap as Bitmap
import qualified Graphics.Rendering.FreeType.Internal.Face as Face
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot as GlyphSlot
import qualified Graphics.Rendering.FreeType.Internal.Size as Size
import qualified Graphics.Rendering.FreeType.Internal.SizeMetrics as SizeMetrics
import qualified Graphics.Rendering.FreeType.Internal.Vector as Vector
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

dpi :: CUInt
dpi = 96

init :: IO FT_Library
init = do
  alloca $ \ptr -> do
    e <- ft_Init_FreeType ptr
    when (e /= 0) $ raise "Error in ft_Init_FreeType"
    peek ptr

fini :: FT_Library -> IO ()
fini lib = do
  e <- ft_Done_FreeType lib
  when (e /= 0) $ raise "Error in ft_Done_FreeType"

convert :: FT_Library -> String -> Int -> String -> IO FontMeta
convert lib inPath size outPath = do
  --
  face <- withCString inPath $ \cstr ->
    alloca $ \ptr -> do
      e <- ft_New_Face lib cstr 0 ptr
      when (e /= 0) $ raise "Error in ft_New_Face"
      peek ptr
  --
  do
    e <- ft_Set_Char_Size face 0 (shiftL ((§) size) 6) dpi dpi
    when (e /= 0) $ raise "Error in ft_Set_Char_Size"
  --
  faceSize <- peek $ Face.size face
  sizeMetrics <- peek $ Size.metrics faceSize
  let
    ascender = (§) (SizeMetrics.ascender sizeMetrics `div` 64)
    descender = (§) (SizeMetrics.descender sizeMetrics `div` 64)
  --
  boxes <- for charSet $ \char -> do
    let code = fromEnum char
    e <- ft_Load_Char face ((§) code) ft_LOAD_RENDER
    when (e /= 0) $ raise "Error in ft_Load_Char"
    glyph <- peek $ Face.glyph face
    bitmap <- peek $ GlyphSlot.bitmap glyph
    let
      width = (§) (Bitmap.pitch bitmap)
      height = (§) (Bitmap.rows bitmap)
    --
    return $ Box width height
  --
  let (Container dimX dimY, places) = fitBest boxes
  mpixels <- MSV.replicate (dimX * dimY) 0
  --
  gs <- for (zip charSet places) $ \(char, Place placeX placeY) -> do
    let code = fromEnum char
    e <- ft_Load_Char face ((§) code) ft_LOAD_RENDER
    when (e /= 0) $ raise "Error in ft_Load_Char"
    glyph <- peek $ Face.glyph face
    bitmap <- peek $ GlyphSlot.bitmap glyph
    left <- peek $ GlyphSlot.bitmap_left glyph
    top <- peek $ GlyphSlot.bitmap_top glyph
    advance26p6 <- peek $ GlyphSlot.advance glyph
    let
      width = Bitmap.pitch bitmap
      height = Bitmap.rows bitmap
      buffer = Bitmap.buffer bitmap
    --
    for_ [0..width - 1] $ \i ->
      for_ [0..height - 1] $ \j -> do
        let offset = dimX * (placeY + (§) j) + placeX + (§) i
        byte <- peek (buffer `plusPtr` (§) (width * j + i))
        --
        MSV.write mpixels offset byte
    --
    return $
      ((§) code, GlyphMeta {
        advanceX = (§) (Vector.x advance26p6 `div` 64),
        renderOffset = (<§>) (V2 left (-top)),
        box = (<§>) (V2 width height),
        place = V2 placeX placeY
      })
  --
  pixels <- SV.unsafeFreeze mpixels
  let
    image :: Juicy.Image Juicy.Pixel8
    image = Juicy.Image dimX dimY pixels
  --
  Juicy.writePng outPath image
  do
    e <- ft_Done_Face face
    when (e /= 0) $ raise "Error in ft_Done_Face"
  --
  print (ascender, descender)
  return $
    FontMeta {
      dimensions = V2 dimX dimY,
      ascender,
      descender,
      glyphs = M.fromList gs
    }
