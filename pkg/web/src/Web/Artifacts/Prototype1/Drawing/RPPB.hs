module Web.Artifacts.Prototype1.Drawing.RPPB where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified GLES                                      as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data RPPB = RPPB { -- Regular Paletted Prog Bits
  loc_texDimensions :: GL.UniformLocation,
  loc_scrDimensions :: GL.UniformLocation,
  loc_texPlace      :: GL.UniformLocation,
  loc_scrPlace      :: GL.UniformLocation,
  loc_scrBox        :: GL.UniformLocation,
  loc_texBox        :: GL.UniformLocation,
  attr_interp       :: GL.GLUInt
}
