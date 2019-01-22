module Web.Artifacts.Game where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Web.GFX'GLES ()
import Web.Platform ()
import Web.Stage.ChangeCursor ()
import Web.Stage.ControlSound ()
import Web.Stage.DetermineInput ()
import Web.Stage.Prerequisites ()
import Web.Stage.SystemLibraries ()
import Web.WND'Canvas ()
import qualified Heroes.Game
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

main' :: IO ()
main' = Heroes.Game.main'
