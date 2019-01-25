{-# OPTIONS_GHC -Wno-orphans #-}
module Web.GFX'GLES () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation.Scene                                   (Actor)
import Animation.Scene                                   (Prop)
import Battle                                            (ObstacleId)
import Heroes.Drawing                                    (CopySpec(..))
import Heroes.Drawing                                    (StaticSprite(..))
import Heroes.Drawing.Utilities                          (makeTexture)
import Heroes.Drawing.Utilities                          (makePaletteTexture)
import Heroes.Drawing.Quad                               (QBuffer)
import Heroes.H3.Misc                                    (oImgName)
import Heroes.GFX
import Heroes.UI                                         (fieldCenter)
import Web
import Web.Platform
import Web.WND'Canvas ()
import qualified GLES                                      as GL
import qualified Heroes.Cell                               as Cell
import qualified Heroes.Drawing                            as Drawing
import qualified Heroes.Drawing.OneColor                   as OneColor
import qualified Heroes.Drawing.Paletted                   as Paletted
import qualified Heroes.Drawing.Quad                       as Quad
import qualified Heroes.Drawing.Regular                    as Regular
import qualified Heroes.FilePath                           as FilePath
import qualified Web.GLES                                  as GL
import qualified Web.Image                                 as Image
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
import qualified JavaScript.TypedArray                     as TypedArray
import qualified JavaScript.Web.AnimationFrame             as AF
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data WebRenderer = WebRenderer GL.Ctx QBuffer

instance GFX'Types where
  type StaticSprite = Drawing.StaticSprite
  type ComplexSprite = Drawing.ComplexSprite

instance GFX where
  type Renderer = WebRenderer
  with (Deps {..}) next = do
    ctx <- GL.getWebGLContext window
    qBuffer <- Quad.createBuffer ctx
    background <- loadStatic ctx FilePath.background
    cellShaded <- loadStatic ctx FilePath.cellShaded
    cellOutline <- loadStatic ctx FilePath.cellOutline
    --
    let
      allObstacles = [minBound .. maxBound]
    --
    obstacleResourceMap <- (M.fromList <$>) $ for allObstacles $ \k -> do
      v <- loadStatic ctx $ FilePath.staticPathOf (oImgName k)
      return (k, v)
    --
    let
      obstacles t = obstacleResourceMap M.! t
      renderer = WebRenderer ctx qBuffer
      staticResources = StaticResources {..}
      prov = Prov {..}
    --
    Regular.with ctx qBuffer $ \regular ->
      Paletted.with ctx qBuffer $ \paletted ->
      OneColor.with ctx qBuffer $ \oneColor ->
      next $ (, prov) $ run regular paletted oneColor ctx staticResources
  --
  loadComplexSprite (WebRenderer ctx _) meta path = do
    image <- Image.load path
    atlasTexture <- makeTexture ctx image
    let palette = meta ^. palette_
    paletteArray' <- TypedArray.create 1024
    for_ [0..255] $ \i -> do
      let V4 r g b a = if i > 8
                      then (<§>) (palette ! i)
                      else 0
      TypedArray.unsafeSetIndex (i * 4 + 0) r paletteArray'
      TypedArray.unsafeSetIndex (i * 4 + 1) g paletteArray'
      TypedArray.unsafeSetIndex (i * 4 + 2) b paletteArray'
      TypedArray.unsafeSetIndex (i * 4 + 3) a paletteArray'
    --
    paletteArray <- freezeUint8Array paletteArray'
    paletteTexture <- makePaletteTexture ctx paletteArray
    return $ Drawing.ComplexSprite { .. }
  destroyComplexSprite _ = return () -- XXX
  --

--------------------------------------------------------------------------------

loadStatic :: GL.Ctx -> String -> IO Drawing.StaticSprite
loadStatic ctx path = do
  img <- Image.load path
  w <- Image.width img
  h <- Image.height img
  texture <- makeTexture ctx img
  let dimensions = (<§>) $ V2 w h
  return $ StaticSprite { texture, dimensions }

fromActor ::
  Actor ->
  Paletted.Cmd
fromActor actor = Paletted.Cmd sprite spec
  where
  sprite = actor ^. _sprite
  frame = (sprite ^. meta_ . groups_) & (! g) & (! f) -- XXX partial...
  f = actor ^. _frameN
  g = actor ^. _groupN
  -- @copypaste from Native.Stage.PrepareForDrawing_.toCopy
  facing = actor ^. _facing
  screenPlace = (<§>) ((actor ^. _position) .+^ offset .-^ (V2 0 (actor ^. _height)))
  offset = (frame ^. offset_) * sign
  sign = case facing of
    West -> V2 (-1) 1
    East -> 1
  spec = CopySpec {
    box = (<§>) (frame ^. box_),
    place = (<§>) (frame ^. place_),
    screenPlace,
    screenBox = (<§>) ((frame ^. box_) * sign)
  }

run ::
  With (Handler Regular.Cmd) ->
  With (Handler Paletted.Cmd) ->
  With (Handler OneColor.Cmd) ->
  GL.Ctx ->
  StaticResources ->
  In ->
  IO ()
run regular paletted oneColor ctx staticResources (In {..}) = do
  let
    curtain = 255 * scene ^. _curtain
    comparingY = (comparing . view) (_position . _y)
    actors = sortBy comparingY $ M.elems $ scene ^. _actors
    props = M.assocs $ scene ^. _props
    StaticResources {..} = staticResources
    --
    bgCmd = fullCopy background 0
    outline = toRegularCmd cellOutline <$> lightHexes
    shaded = toRegularCmd cellShaded <$> darkHexes
    regularCmds = [bgCmd] <> (fromProp <$> props) <> shaded <> outline
    toRegularCmd sprite hex =
      fullCopy sprite $
        (<§>) (fieldCenter .+^ Cell.fromHex hex)
    --
    fromProp :: (ObstacleId, Prop) -> Regular.Cmd
    fromProp (o, prop) = Regular.Cmd sprite spec
      where
      sprite = obstacles (o ^. otype_)
      sign = case prop ^. _facing of
        West -> V2 (-1) 1
        East -> 1
      screenPlace = (<§>) (prop ^. _position)
      spec = CopySpec {
        box = sprite ^. dimensions_,
        place = 0,
        screenPlace,
        screenBox = (sprite ^. dimensions_) * sign
      }
    --
    palettedCmds = fromActor <$> actors
  --
  GL.glEnable ctx GL.gl_BLEND
  GL.glBlendFunc ctx GL.gl_SRC_ALPHA GL.gl_ONE_MINUS_SRC_ALPHA
  --
  regular $ \draw ->
    for_ regularCmds draw
  --
  paletted $ \draw ->
    for_ palettedCmds draw
  --
  oneColor $ \draw -> do
    let color = V4 0 0 0 curtain
    draw $ OneColor.Cmd { color, box = Nothing, place = 0 }
  --
  void $ AF.waitForAnimationFrame -- XXX does not belong here.

fullCopy :: Drawing.StaticSprite -> Point V2 Float -> Regular.Cmd
fullCopy sprite screenPlace = Regular.Cmd sprite spec
  where
  spec = CopySpec {
    box = sprite ^. dimensions_,
    screenBox = sprite ^. dimensions_,
    place = 0,
    screenPlace
  }
