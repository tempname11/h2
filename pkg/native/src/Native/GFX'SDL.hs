{-# OPTIONS_GHC -Wno-orphans #-}
module Native.GFX'SDL where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Scaling
import Heroes.GFX
import Heroes.UI
import Native
import Native.Platform ()
import Native.Utils                                      (createPalettedSurface)
import Native.GFX'SDL.Common
import Native.WND'SDL ()
import qualified Heroes.Cell                               as Cell
import qualified Native.GFX'SDL.Prepare                    as Prepare
import qualified Native.GFX'SDL.StaticResources            as Resource
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import SDL                                               (($=))
import qualified Codec.Picture                             as Juicy
import qualified Data.Vector.Storable                      as SV
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance GFX where
  type Renderer = SDL.Renderer
  with (Deps {..}) next = do
    renderer <- SDL.createRenderer window (-1) rendererConfig
    staticResources <- Resource.init renderer
    let pDeps = Prepare.Deps {..}
    Prepare.with pDeps $ \pRun -> do
      let
        draw = \(in_@In {..}) -> do
          Prepare.Out {..} <- pRun (Prepare.In {..})
          run pDeps in_ drawingAct
      next $ Prov {..}
    Resource.fini staticResources
    SDL.destroyRenderer renderer
  --
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
    return $ ComplexSprite'SDL {
      surface = surface,
      palette = palette,
      groups  = meta ^. groups_
    }
  destroyComplexSprite s = SDL.freeSurface (s ^. surface_)
  --

data Style
  = Outline
  | Shaded

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False }

run :: Prepare.Deps -> In -> DrawingAct -> IO ()
run (Prepare.Deps {..}) (In {..}) (DrawingAct {..}) = do
  SDL.rendererDrawColor renderer $= black
  SDL.clear renderer
  drawStatic 0 (staticResources ^. background_)
  for_ outline $ drawHex Outline
  for_ shaded  $ drawHex Shaded
  for_ copies  $ copy
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  SDL.rendererDrawColor renderer $= V4 0 0 0 curtain
  SDL.fillRect renderer Nothing
  SDL.present renderer
  where
  --
  drawStatic :: Point V2 CInt -> StaticSprite -> IO ()
  drawStatic p sprite = SDL.copy renderer texture rect0 rect1
    where
    dimensions = sprite ^. dimensions_
    texture = sprite ^. texture_
    rect0 = Just $ SDL.Rectangle 0 dimensions
    rect1 = Just $ SDL.Rectangle p (rescaled dimensions)
  --
  drawHex :: Style -> Hex -> IO ()
  drawHex style hex = drawStatic p sprite
    where
    p = rescaled $ fieldCenter .+^ (Cell.fromHex hex)
    sprite = case style of
            Shaded  -> (staticResources ^. cellShaded_)
            Outline -> (staticResources ^. cellOutline_)
  --
  copy :: CopyCommand -> IO ()
  copy it =
    SDL.copyEx renderer texture src dst 0 Nothing flips
    where
    texture = it ^. texture_
    src     = it ^. src_
    dst     = it ^. dst_
    flips   = it ^. flips_
