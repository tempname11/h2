module Heroes.Root.TitleScreen where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Font                                       (Font(..))
import Heroes.UI                                         (viewportSize)
import qualified Heroes.Color                              as Color
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

inBounds :: (Ord a, Num a) => Point V2 a -> V2 a -> Maybe (Point V2 a) -> Bool
inBounds (P (V2 pX pY)) (V2 bX bY) = \case
  Nothing -> False
  Just (P (V2 x y)) ->
    (x >= pX && x < pX + bX) &&
    (y >= pY && y < pY + bY)

mkDrawCallback :: Maybe (Point V2 Int) -> GFX.DrawCallback
mkDrawCallback mouseAt _ _ text _ (GFX.StaticResources {..}) = do
  let
    viewportCenter = P $ (<§>) viewportSize * 0.5
    place = viewportCenter .-^ box * 0.5
    (box, prepared) =
      Text.prepare
        (fonts ! Font'CompassPro24)
        "Welcome, traveler!"
    --
    fontTestCmd = Text.Cmd {
      prepared,
      color = (
        if inBounds place box (fmap2 (§) mouseAt)
        then Color.yellow
        else Color.white
      ),
      screenPlace = place
    }
  text ($ fontTestCmd)

run :: In -> Data -> IO Root.ScreenOut
run (In {..}) (Data {}) = do
  let
    drawCallback = mkDrawCallback (fullInput ^. #mouseAt)
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
