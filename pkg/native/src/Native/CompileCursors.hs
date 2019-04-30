module Native.CompileCursors where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Native.Platform ()
import Native.WND'SDL () -- XXX
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.Griffin                            as Griffin
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Codec.Picture                             as Juicy
import qualified Data.ByteString                           as B
import qualified Data.IntMap.Strict                        as IntMap
import qualified Data.Vector                               as V
import qualified Data.Vector.Storable                      as SV
import qualified Data.Vector.Storable.Mutable              as MSV
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

-- XXX this shares a good deal of code with Atlas, but who cares?

assetsPath :: String
assetsPath = "h3-assets/"

type Poke m = Int -> V2 CInt -> ByteString -> m ()

data Blueprint m = Blueprint {
  pokeCombinator :: Poke m -> m (),
  dimensions :: V2 CInt,
  count :: Int,
  palette :: SV.Vector (V4 Word8)
}

main' :: IO ()
main' = do
  let
    files =
      [
        (0, assetsPath <> "Defs/CRCOMBAT.def"),
        (1, assetsPath <> "Defs/crdeflt.def"),
        (2, assetsPath <> "Defs/Crspell.def")
      ]
  
  for_ files $ \(fileNumber :: Int, fileName) -> do
    buf <- B.readFile fileName
    Blueprint { pokeCombinator, dimensions, count, palette } <-
      case parse buf of
        Left str -> raise str
        Right b -> return b
    --
    let
      byteSize = (§) (w * h * 4)
      (V2 w h) = dimensions
    --
    mpixels <- MSV.replicate (count * byteSize) 0
    let
      poke j (V2 x y) bytes = generateM_ (B.length bytes) $ \i -> do
        let
          offset = (§) (4 * (x + y * w)) + (§) (j * byteSize)
          paletteColor = (SV.!) palette byte
          -- color "de-keying"
          V4 r g b a = if paletteColor == V4 0 255 255 255 then 0 else paletteColor
          byte = (§) $ B.index bytes i
        --
        MSV.write mpixels (offset + (4 * i) + 0) r
        MSV.write mpixels (offset + (4 * i) + 1) g
        MSV.write mpixels (offset + (4 * i) + 2) b
        MSV.write mpixels (offset + (4 * i) + 3) a
    --
    pokeCombinator poke
    allPixels <- SV.freeze mpixels
    generateM_ count $ \j -> do
      let
        framePixels = SV.slice (j * byteSize) byteSize allPixels
        image = Juicy.Image @Juicy.PixelRGBA8 ((§) w) ((§) h) framePixels
      --
      Juicy.writePng (FilePath.cursorPathOf (show fileNumber <> "-" <> show j)) image
    --

parse :: forall m. Monad m => ByteString -> Either String (Blueprint m)
parse buf = go <$> Griffin.parse buf
  where
  go data_ = Blueprint { pokeCombinator, dimensions, count, palette }
    where
    pokeCombinator poke = sequence_ actions
      where
      actions = zipWith f'' (IntMap.elems frames) ints
      ints = [0..] :: [Int]
      f'' frame i = sequence_ subActions
        where
        subActions = zipWith f''' rows ints
        rows = V.toList (Griffin.fRows frame)
        x = (§) $ Griffin.fOffsetX frame
        y = (§) $ Griffin.fOffsetY frame
        f''' row r = poke i position row
          where
          position :: V2 CInt
          position = V2 x (y + (§) r)
    dimensions = (<§>) (V2 w h)
      where
      w = Griffin.hWidth header
      h = Griffin.hHeight header
    count = IntMap.size frames
    palette = Griffin.hPalette header
    frames = Griffin.dFrames data_
    header = Griffin.dHeader data_
