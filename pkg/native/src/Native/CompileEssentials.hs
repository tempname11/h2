module Native.CompileEssentials where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Essentials
import Heroes.Font                                       (fontNameOf)
import Heroes.Font                                       (fontSizeOf)
import Heroes.SpriteMeta                                 (SpriteMeta(..))
import Heroes.UI
import Native
import Native.Platform ()
import Native.WND'SDL () -- XXX
import qualified Heroes.Atlas                              as Atlas
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.H3                                 as H3
import qualified Native.CompileEssentials.Fonts            as Fonts
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Codec.Picture                             as Juicy
import qualified Data.ByteString                           as B
import qualified Data.Map.Strict                           as M
import qualified Data.Vector.Storable                      as SV
import qualified Data.Vector.Storable.Mutable              as MSV
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

assetsPath :: String
assetsPath = "h3-assets/"

main' :: IO ()
main' = do
  convertedFonts <- do
    lib <- Fonts.init
    result <- for genum $ \f -> do
      let
        name = fontNameOf f
        size = fontSizeOf f
        ttfPath = assetsPath <> "fonts/" <> name <> ".ttf"
        pngPath = FilePath.fontAtlasPathOf name
      --
      Fonts.convert lib ttfPath size pngPath
    Fonts.fini lib
    return result
  --
  convertedSfx <- for genum $
    \s -> convert (sfxGroundOffset s) (H3.sDefName s)
  --
  convertedCreatures <- for genum $
    \c -> convert creatureGroundOffset (H3.cDefName c)
  --
  let
    cs = M.fromList (zip genum convertedCreatures)
    ss = M.fromList (zip genum convertedSfx)
    fs = M.fromList (zip genum convertedFonts)
    essentials = Essentials {
      creatureMeta = \c -> cs ! c,
      sfxMeta = \s -> ss ! s,
      fontMeta = \f -> fs ! f
    }
    output = serializeWith put essentials
  --
  B.writeFile FilePath.essentialsBin1 output

--------------------------------------------------------------------------------

convert :: V2 CInt -> String -> IO SpriteMeta
convert groundOffset defName = do
  let defPath = assetsPath <> "Defs/" <> defName <> ".def"
  let pngPath = FilePath.pngPathOf defName
  putStrLn $ "Converting... " <> defPath <> " to " <> pngPath
  buf <- B.readFile defPath
  blueprint <- case Atlas.parse groundOffset buf of
                 Left str -> raise str
                 Right b -> return b
  --
  let (V2 w h)       = Atlas.dimensions blueprint
      palette        = Atlas.palette blueprint
      pokeCombinator = Atlas.pokeCombinator blueprint
      groups         = Atlas.groups blueprint
      size = (§) (w * h)
  --
  mpixels <- MSV.replicate size 0
  --
  let poke (V2 x y) bytes = generateM_ (B.length bytes) go
        where
        offset = (§) (x + y * w)
        go :: Int -> IO ()
        go i = MSV.write mpixels (offset + i) byte
          where byte = B.index bytes i
  --
  pokeCombinator poke
  --
  pixels <- SV.unsafeFreeze mpixels
  --
  let
    image = Juicy.Image ((§) w) ((§) h) pixels
    image :: Juicy.Image Juicy.Pixel8
    meta = SpriteMeta {
      dimensions = V2 w h,
      palette = palette,
      groups = groups
    }
  --
  Juicy.writePng pngPath image
  return meta
