{-# OPTIONS_GHC -Wno-orphans #-}
module Native.GFX'SDL () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Platform
import Heroes.Scaling
import Heroes.Subsystems.GFX
import Heroes.UI
import Native
import Native.Platform ()
import qualified Heroes.Cell                               as Cell
import qualified Native.ResourceIO                         as Resource
import qualified Native.Stage.PrepareForDrawing_           as P
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.String                                       (fromString)
import SDL                                               (($=))
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance GFX where
  with next = do
    SDL.initialize [
        SDL.InitAudio,
        SDL.InitVideo, 
        SDL.InitEvents
      ]
    window <- SDL.createWindow (fromString "Fight!") windowConfig
    renderer <- SDL.createRenderer window (-1) rendererConfig
    staticResources <- Resource.init renderer
    let prov = Prov {..}
    let pDeps = P.Deps {..}
    P.with pDeps $
      \pRun -> next $ (, prov) $
        \(in_@In {..}) -> do
          P.Out {..} <- pRun (P.In {..})
          run pDeps in_ drawingAct
    Resource.fini staticResources
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

windowConfig :: SDL.WindowConfig
windowConfig = SDL.defaultWindow
  { SDL.windowPosition = SDL.Absolute (P $ V2 100 100)
  , SDL.windowInitialSize = (<§>) viewportSize }

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False }

data Style
  = Outline
  | Shaded

run :: P.Deps -> In -> DrawingAct -> IO ()
run (P.Deps {..}) (In {..}) (DrawingAct {..}) = do
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

  drawHex :: Style -> Hex -> IO ()
  drawHex style hex = drawStatic p sprite
    where
    p = rescaled $ fieldCenter .+^ (Cell.fromHex hex)
    sprite = case style of
            Shaded  -> (staticResources ^. cellShaded_)
            Outline -> (staticResources ^. cellOutline_)

  copy :: CopyCommand -> IO ()
  copy it =
    SDL.copyEx renderer texture src dst 0 Nothing flips
    where
    texture = it ^. texture_
    src     = it ^. src_
    dst     = it ^. dst_
    flips   = it ^. flips_

