module Web.DrawingAct where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import qualified Web.Drawing.Regular                       as Regular
import qualified Web.Drawing.Paletted                      as Paletted
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data DrawingAct = DrawingAct {
  curtain      :: Float,
  regularCmds  :: [Regular.Cmd],
  palettedCmds :: [Paletted.Cmd]
}
