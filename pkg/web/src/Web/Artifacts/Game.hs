module Web.Artifacts.Game where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Web.Platform ()
import Web.Stage.ChangeCursor ()
import Web.Stage.ControlSound ()
import Web.Stage.DetermineInput ()
import Web.Stage.Draw ()
import Web.Stage.Prerequisites ()
import Web.Stage.SystemLibraries ()
import qualified Heroes.Game
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

main' :: IO ()
main' = Heroes.Game.main'
