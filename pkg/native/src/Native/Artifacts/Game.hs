module Native.Artifacts.Game where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Native.Platform ()
import Native.Stage.ChangeCursor ()
import Native.Stage.ControlSound ()
import Native.Stage.DetermineInput ()
import Native.Stage.Draw ()
import Native.Stage.Prerequisites ()
import Native.Stage.SystemLibraries ()
import qualified Heroes.Game
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

main' :: IO ()
main' = Heroes.Game.main'
