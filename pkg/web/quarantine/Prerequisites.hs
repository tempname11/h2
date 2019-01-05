{-# LANGUAGE JavaScriptFFI #-}
module Web.Prerequisites (
  with,
  Prov (..),
  Deps (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Essentials                                 (Essentials(..))
import Web
import Web.Drawing
import Web.Drawing.Utilities                             (makeTexture)
import qualified Heroes.Essentials                         as Essentials
import qualified Native.FilePath                           as FilePath
import qualified Stage.Links                               as L
import qualified Web.GLES                                  as GL
import qualified Web.Image                                 as Image
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified JavaScript.Web.XMLHttpRequest             as XHR
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  theContext :: L.TheContext
}

data Prov = Prov {
  essentials :: L.Essentials,
  background :: L.Background,
  cellShaded  :: L.CellShaded,
  cellOutline :: L.CellOutline
}

--------------------------------------------------------------------------------
with :: Deps -> (Prov -> IO a) -> IO a
with (Deps {..}) next = do
  essentials <- loadEssentials
  background <- loadStatic theContext FilePath.background
  cellShaded <- loadStatic theContext FilePath.cellShaded
  cellOutline <- loadStatic theContext FilePath.cellOutline
  next $ Prov {..}
--------------------------------------------------------------------------------
loadEssentials :: IO Essentials
loadEssentials = do
  let request = simpleXHR path
      path = FilePath.essentialsBin1

  result <- XHR.contents <$> XHR.xhrByteString request
  case parseWith Essentials.getIt <$> result of
    Nothing -> raise "Couldn't load the Essentials."
    Just (Left str) -> raise str
    Just (Right x) -> return x
--------------------------------------------------------------------------------
loadStatic :: GL.Context -> String -> IO StaticSprite
loadStatic ctx path = do
  img <- Image.load path
  w <- Image.width img
  h <- Image.height img
  texture <- makeTexture ctx img
  let dimensions = (<§>) $ V2 w h
  return $ StaticSprite { texture, dimensions }

