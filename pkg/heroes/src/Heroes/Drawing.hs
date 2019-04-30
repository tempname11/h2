module Heroes.Drawing where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import GLES                                              (GLES)
import Heroes
import Heroes.FontMeta                                   (FontMeta)
import Heroes.SpriteMeta                                 (SpriteMeta)
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data CopySpec = CopySpec {
  box         :: V2 Float,
  screenBox   :: V2 Float,
  place       :: Point V2 Float,
  screenPlace :: Point V2 Float
} deriving (Generic)

data StaticSprite = StaticSprite {
  texture    :: GL.Texture,
  dimensions :: V2 Float
} deriving (Generic)

data ComplexSprite = ComplexSprite {
  atlasTexture :: GL.Texture,
  paletteTexture :: GL.Texture,
  meta :: SpriteMeta
} deriving (Generic)

data FontAtlas = FontAtlas {
  texture :: GL.Texture,
  meta :: FontMeta
} deriving (Generic)

clear :: GLES => GL.Ctx -> IO ()
clear ctx = do
  GL.glClearColor ctx 0 0 0 1
  GL.glClear ctx GL.gl_COLOR_BUFFER_BIT
