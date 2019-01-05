{-# LANGUAGE TemplateHaskell #-}
module Web.Drawing where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import qualified Web.GLES                                  as GL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data CopySpec = CopySpec {
  box         :: V2 Float,
  screenBox   :: V2 Float,
  place       :: Point V2 Float,
  screenPlace :: Point V2 Float
}

--------------------------------------------------------------------------------

clear :: GL.Context -> IO ()
clear ctx = do
  GL.clearColor ctx 0 0 0 1
  GL.clear ctx GL.gl_COLOR_BUFFER_BIT

--------------------------------------------------------------------------------

makeShorthands ''CopySpec
