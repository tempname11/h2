{-# LANGUAGE TemplateHaskell #-}
module Heroes.Drawing where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import GLES                                              (GLES)
import Heroes
import Heroes.SpriteMeta                                 (Meta)
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data CopySpec = CopySpec {
  box         :: V2 Float,
  screenBox   :: V2 Float,
  place       :: Point V2 Float,
  screenPlace :: Point V2 Float
}

makeShorthands ''CopySpec

--------------------------------------------------------------------------------

clear :: GLES => GL.Ctx -> IO ()
clear ctx = do
  GL.glClearColor ctx 0 0 0 1
  GL.glClear ctx GL.gl_COLOR_BUFFER_BIT

data StaticSprite = StaticSprite {
  texture    :: GL.Texture,
  dimensions :: V2 Float
}

data ComplexSprite = ComplexSprite {
  atlasTexture :: GL.Texture,
  paletteTexture :: GL.Texture,
  meta :: Meta
}

makeShorthands ''ComplexSprite
makeShorthands ''StaticSprite
