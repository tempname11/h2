module Web.Artifacts.Prototype1.Drawing.Paletted where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Web.Artifacts.Prototype1.Drawing.RPPB
import Web.GLES ()
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.JSString as JSString
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Prog = Prog {
  -- a 'Prog' is a GL.Program bundled together
  -- with the necessary information about it.
  program        :: GL.Program,
  loc_texAtlas   :: GL.UniformLocation,
  loc_texPalette :: GL.UniformLocation,
  rppb           :: RPPB
}

toProg :: GL.Ctx -> GL.Program -> IO Prog
toProg ctx program = do
  --
  attr_interp <- (§) <$> -- Int32 vs Word32 for some reason
    GL.glGetAttribLocation ctx program (JSString.pack "interp")
  --
  let locate name = GL.glGetUniformLocation ctx program (JSString.pack name)
  --
  loc_texDimensions <- locate "texDimensions"
  loc_scrDimensions <- locate "scrDimensions"
  loc_texPlace      <- locate "texPlace"
  loc_scrPlace      <- locate "scrPlace"
  loc_scrBox        <- locate "scrBox"
  loc_texBox        <- locate "texBox"
  loc_texAtlas      <- locate "texAtlas"
  loc_texPalette    <- locate "texPalette"
  --
  let rppb = RPPB { .. }
  return $ Prog { .. }
