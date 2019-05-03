module Heroes.Root.TitleScreen where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Font                                       (Font(..))
import Heroes.UI                                         (viewportSize)
import qualified Heroes.Input                              as Input
import qualified Heroes.Drawing.Text                       as Text
import qualified Heroes.GFX                                as GFX
import qualified Heroes.Root.Common                        as Root
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data In = In {
  fullInput :: Input.Full,
  dispatch :: Root.Action -> IO ()
} deriving (Generic)

data Data = Data {}
  deriving (Generic)

init :: IO Data
init = do
  return $ Data {}

mkDrawCallback :: () -> GFX.DrawCallback
mkDrawCallback _ _ _ text _ (GFX.StaticResources {..}) = do
  let
    (measurements, prepared) =
      Text.prepare
        (fonts ! Font'CompassPro24)
        "Welcome, traveler!"
    --
    fontTestCmd = Text.Cmd {
      prepared,
      screenPlace = P $ ((<§>) viewportSize - measurements) * 0.5
    }
  text $ \draw ->
    draw fontTestCmd

run :: In -> Data -> IO Root.ScreenOut
run (In {..}) (Data {}) = do
  let
    drawCallback = mkDrawCallback ()
    soundCommands = V.empty
    intent = Nothing
    loadRequests = empty
    (exit, start) =
      let Input.Full {..} = fullInput
      in (
        quitEvent || keyUp Input.Key'Escape,
        keyUp Input.Key'Enter
      )
  when exit $ dispatch Root.Action'ExitScreen
  when start $ dispatch Root.Action'StartBattle
  return (Root.ScreenOut {..})
