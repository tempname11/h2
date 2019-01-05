module Native.Artifacts.CompileEssentials where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Essentials
import Heroes.UI
import Native
import Native.Platform ()
import qualified Heroes.Atlas                              as Atlas
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.H3                                 as H3
import qualified Heroes.SpriteMeta                         as Meta
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Codec.Picture                             as Juicy
import qualified Data.ByteString                           as B
import qualified Data.Map.Strict                           as M
import qualified Data.Vector.Storable                      as SV
import qualified Data.Vector.Storable.Mutable              as MSV
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

main' :: IO ()
main' = do
  let allCreatures = [minBound .. maxBound]
      allSfx = [minBound .. maxBound]
  convertedSfx <- for allSfx $
    \s -> convert sfxGroundOffset (H3.sDefName s)
  convertedCreatures <- for allCreatures $
    \c -> convert creatureGroundOffset (H3.cDefName c)
  let cs = M.fromList (zip allCreatures convertedCreatures)
      ss = M.fromList (zip allSfx convertedSfx)
  let essentials = Essentials {
    creatureMeta = \c -> cs ! c,
    sfxMeta = \s -> ss ! s
  }
      output = serializeWith putIt essentials
  B.writeFile FilePath.essentialsBin1 output

--------------------------------------------------------------------------------

convert :: V2 CInt -> String -> IO Meta.Meta
convert groundOffset defName = do
  let defPath = FilePath.h3 <> "Defs/" <> defName <> ".def"
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
  let image = Juicy.Image ((§) w) ((§) h) pixels
      image :: Juicy.Image Juicy.Pixel8

  let meta = Meta.Meta {
    Meta.dimensions = V2 w h,
    Meta.palette = palette,
    Meta.groups = groups
  }
  --
  Juicy.writePng pngPath image
  return meta
