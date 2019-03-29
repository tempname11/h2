{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Native.GFX'SDL.StaticResources where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.H3.Misc 
import Heroes.Platform
import Native 
import Native.GFX'SDL.Common
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.GFX                                as GFX
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import SDL                                               (($=))
import qualified Data.Map.Strict                           as M
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

init :: Platform => SDL.Renderer -> IO GFX.StaticResources
init renderer = do
  let loadSX = loadStatic renderer
      loadS = loadSX noOp noOp
      noOp = const $ return ()
      --
      obstacleS :: SDL.Surface -> IO ()
      obstacleS surface =
        SDL.surfaceColorKey surface $= Just (V4 0x00 0xFF 0xFF 0xFF)
      --
      cellS :: SDL.Surface -> IO ()
      cellS surface =
        SDL.surfaceColorKey surface $= Just (V4 0xFF 0x00 0xFF 0xFF)
      --
      cellT :: SDL.Texture -> IO ()
      cellT texture = do
        SDL.textureAlphaMod texture $= 0x80
        SDL.textureColorMod texture $= V3 0x00 0x00 0x00
  --
  background  <- loadS FilePath.background
  cellOutline <- loadSX cellS noOp FilePath.cellOutline
  cellShaded  <- loadSX cellS cellT FilePath.cellShaded
  --
  let allObstacles = [minBound .. maxBound]
      loadObstacle k = do
        v <- loadSX obstacleS noOp (FilePath.staticPathOf (oImgName k))
        return (k, v)
  --
  obstacleResourceMap <- M.fromList <$> for allObstacles loadObstacle
  --
  let obstacles t = obstacleResourceMap M.! t
  --
  return $ GFX.StaticResources {..}

loadStatic :: SDL.Renderer ->
          (SDL.Surface -> IO ()) ->
          (SDL.Texture -> IO ()) ->
          String -> IO StaticSprite'SDL
loadStatic renderer surfaceIO textureIO path = do
  surface <- SDL.loadBMP path
  void $ surfaceIO surface
  dimensions <- SDL.surfaceDimensions surface
  texture <- SDL.createTextureFromSurface renderer surface
  void $ textureIO texture
  SDL.freeSurface surface
  return $ StaticSprite'SDL {
    texture = texture,
    dimensions = dimensions
  }

destroyStatic :: StaticSprite'SDL -> IO ()
destroyStatic s = SDL.destroyTexture (s ^. texture_)

fini ::
  GFX.StaticResources ->
  IO ()
fini static = do
  destroyStatic (static ^. background_)
  destroyStatic (static ^. cellShaded_)
  destroyStatic (static ^. cellOutline_)
