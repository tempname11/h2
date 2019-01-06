{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Stage.Draw () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Platform
import Heroes.Scaling
import Heroes.UI
import Native
import Native.Platform ()
import Stage.Draw
import qualified Heroes.Cell                               as Cell
import qualified Native.Stage.PrepareForDrawing_           as P
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import SDL                                               (($=))
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Draw where
  with (deps@Deps {..}) next =
    P.with (P.Deps {..}) $
      \pRun -> next $
        \(in_@In {..}) -> do
          P.Out {..} <- pRun (P.In {..})
          run deps in_ drawingAct

--------------------------------------------------------------------------------

data Style
  = Outline
  | Shaded

--------------------------------------------------------------------------------

run :: Deps -> In -> DrawingAct -> IO ()
run (Deps {..}) (In {..}) (DrawingAct {..}) = do
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

