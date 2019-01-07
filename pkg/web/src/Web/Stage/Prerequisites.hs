{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Stage.Prerequisites () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Essentials                                 (Essentials)
import Heroes.H3.Misc 
import Heroes.StaticResources                            (StaticResources(..))
import Heroes.UI                                         (viewportSize)
import Stage.Prerequisites
import Web
import Web.Drawing.Utilities                             (makeTexture)
import Web.Platform
import qualified Heroes.Essentials                         as Essentials
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.Platform                           as Platform
import qualified GLES                                      as GL
import qualified Web.Drawing.Quad                          as Quad
import qualified Web.Image                                 as Image
import qualified Web.GLES                                  as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import JavaScript.Web.Canvas                             (Canvas)
import qualified Data.Map.Strict                           as M
import qualified JavaScript.Web.Canvas                     as Canvas
import qualified JavaScript.Web.XMLHttpRequest             as XHR
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
--
instance Prerequisites where
  with _ next = do
    inputProvider <- Canvas.create (viewportSize ^. _x) (viewportSize ^. _y)
    appendCanvasToBody inputProvider
    ctx <- GL.getWebGLContext inputProvider
    qBuffer <- Quad.createBuffer ctx
    --
    essentials <- loadEssentials
    background <- loadStatic ctx FilePath.background
    cellShaded <- loadStatic ctx FilePath.cellShaded
    cellOutline <- loadStatic ctx FilePath.cellOutline
    --
    let allObstacles = [minBound .. maxBound]
    obstacleResourceMap <- (M.fromList <$>) $ for allObstacles $ \k -> do
      v <- loadStatic ctx $ FilePath.staticPathOf (oImgName k)
      return (k, v)
    --
    let
      renderer = WebRenderer ctx qBuffer
      obstacles t = obstacleResourceMap M.! t
      cursorResources = () -- XXX
      staticResources = StaticResources {..}
    next $ Prov {..}

loadEssentials :: IO Essentials
loadEssentials = do
  let request = simpleXHR path
      path = FilePath.essentialsBin1

  result <- XHR.contents <$> XHR.xhrByteString request
  case parseWith Essentials.getIt <$> result of
    Nothing -> raise "Couldn't load the Essentials."
    Just (Left str) -> raise str
    Just (Right x) -> return x

loadStatic :: GL.Ctx -> String -> IO Platform.StaticSprite
loadStatic ctx path = do
  img <- Image.load path
  w <- Image.width img
  h <- Image.height img
  texture <- makeTexture ctx img
  let dimensions = (<§>) $ V2 w h
  return $ WebStaticSprite { texture, dimensions }

foreign import javascript unsafe "document.body.appendChild($1)"
  appendCanvasToBody :: Canvas -> IO ()
