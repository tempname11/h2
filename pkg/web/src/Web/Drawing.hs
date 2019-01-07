{-# LANGUAGE TemplateHaskell #-}
module Web.Drawing where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import GLES                                              (GLES)
import Web
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
