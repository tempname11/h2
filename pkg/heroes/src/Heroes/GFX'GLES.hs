{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Heroes.GFX'GLES () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import GLES                                              (GLES)
import Heroes
import Heroes.Drawing                                    (StaticSprite(..))
import Heroes.Drawing                                    (FontAtlas(..))
import Heroes.Drawing.Utilities                          (makeTexture)
import Heroes.Drawing.Utilities                          (makePaletteTexture)
import Heroes.Drawing.Quad                               (QBuffer)
import Heroes.Essentials                                 (Essentials(..))
import Heroes.Font                                       (Font(..))
import Heroes.GFX
import Heroes.H3.Misc                                    (oImgName)
import Heroes.Platform                                   (Platform)
import qualified GLES                                      as GL
import qualified Heroes.Drawing                            as Drawing
import qualified Heroes.Drawing.OneColor                   as OneColor
import qualified Heroes.Drawing.Paletted                   as Paletted
import qualified Heroes.Drawing.Quad                       as Quad
import qualified Heroes.Drawing.Text                       as Text
import qualified Heroes.Drawing.Regular                    as Regular
import qualified Heroes.Image                              as Image
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.Platform                           as Platform
import qualified Heroes.GLX                                as GLX
import qualified Heroes.WND                                as WND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Renderer'GLES = Renderer'GLES GL.Ctx QBuffer

instance (GLES, Platform, GLX.GLX, WND.WND) => GFX where
  type Renderer = Renderer'GLES
  with (Deps {..}) next = do
    ctx <- GLX.getGLContext window
    qBuffer <- Quad.createBuffer ctx
    fonts <-
      (M.fromList <$>) $
        for genum $
          \f -> (f,) <$>
            loadFontAtlas essentials ctx f
    --
    background <- loadStatic ctx FilePath.background
    cellShaded <- loadStatic ctx FilePath.cellShaded
    cellOutline <- loadStatic ctx FilePath.cellOutline
    obstacleResourceMap <- (M.fromList <$>) $ for genum $ \k -> do
      v <- loadStatic ctx $ FilePath.staticPathOf (oImgName k)
      return (k, v)
    --
    let
      obstacles t = obstacleResourceMap M.! t
      renderer = Renderer'GLES ctx qBuffer
      staticResources = StaticResources {..}
    --
    GL.glEnable ctx GL.gl_BLEND
    GL.glBlendFuncSeparate ctx
      GL.gl_SRC_ALPHA
      GL.gl_ONE_MINUS_SRC_ALPHA
      GL.gl_ONE
      GL.gl_ONE_MINUS_SRC_ALPHA
    --
    id $ 
      Regular.with ctx qBuffer $ \regular ->
      Paletted.with ctx qBuffer $ \paletted ->
      OneColor.with ctx qBuffer $ \oneColor ->
      Text.with ctx qBuffer $ \text ->
        let
          draw callback = do
            Drawing.clear ctx
            callback
              regular
              paletted
              text
              oneColor
              staticResources
            -- XXX proper error logging
            -- GL.glGetError ctx >>= print @Int . (§)
        in next $ Prov {..}
  --
  loadComplexSprite (Renderer'GLES ctx _) meta path = do
    image <- Platform.loadImage path >>= \case
      Right image -> return image
      Left str -> raise str
    atlasTexture <- makeTexture ctx GL.gl_R8 GL.gl_RED image
    let buf = unsafeToBuf (meta ^. #palette)
    paletteTexture <- makePaletteTexture ctx buf
    return $ Drawing.ComplexSprite { .. }
  --
  destroyComplexSprite _ = return () -- XXX

loadStatic ::
  (Platform, GL.GLES) =>
  GL.Ctx ->
  String ->
  IO Drawing.StaticSprite
loadStatic ctx path = do
  image <- Platform.loadImage path >>= \case
    Right image -> return image
    Left str -> raise str
  w <- Image.width image
  h <- Image.height image
  texture <- makeTexture ctx GL.gl_RGBA GL.gl_RGBA image
  let dimensions = (<§>) $ V2 w h
  return $ StaticSprite { texture, dimensions }

loadFontAtlas ::
  (Platform, GL.GLES) =>
  Essentials ->
  GL.Ctx ->
  Font ->
  IO Drawing.FontAtlas
loadFontAtlas (Essentials { fontMeta }) ctx font = do
  image <- Platform.loadImage (FilePath.fontAtlasPathOf font) >>= \case
    Right image -> return image
    Left str -> raise str
  texture <- makeTexture ctx GL.gl_R8 GL.gl_RED image
  return $ FontAtlas { texture, meta = fontMeta font }
