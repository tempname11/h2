module Web.Artifacts.Game where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.GFX'GLES ()
import Web
import Web.GLX ()
import Web.Memory ()
import Web.SND'Audio ()
import Web.WND'Canvas ()
import Web.Platform ()
import Web.Stage.DetermineInput ()
import Web.Stage.Prerequisites ()
import Web.Stage.SystemLibraries ()
import Web.WND'Canvas ()
import qualified Heroes.Game                               as Game
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

main' :: IO ()
main' = Game.main'
