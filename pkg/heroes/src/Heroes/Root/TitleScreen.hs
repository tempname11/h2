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
  dispatch :: Root.Action -> IO (),
  staticResources :: GFX.StaticResources
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

data SBDeps = SBDeps {
  fullInput :: Input.Full,
  staticResources :: GFX.StaticResources
} deriving (Generic)

data SimpleButton = SimpleButton {
  clicked :: Bool,
  cmd :: Text.Cmd
} deriving (Generic)

simpleButton :: SBDeps -> Point V2 Float -> ByteString -> SimpleButton
simpleButton (SBDeps {..}) center string = SimpleButton {..}
  where
  Input.Full {..} = fullInput
  GFX.StaticResources {..} = staticResources
  (box, preparation) =
    Text.prepare
      (fonts ! Font'CompassPro24)
      string
  screenPlace = center .-^ box * 0.5
  hovered = inBounds screenPlace box (fmap2 (§) mouseAt)
  clicked = hovered && mouseDown Input.LMB
  cmd = Text.Cmd {
    preparation,
    color = (
      if hovered
      then Color.yellow
      else Color.white
    ),
    screenPlace
  }

viewportCenter :: Point V2 Float
viewportCenter = P $ (<§>) viewportSize * 0.5

run :: In -> Data -> IO Root.ScreenOut
run (In {..}) (Data {}) = do
  let
    startButton :: SimpleButton
    startButton =
      simpleButton
        (SBDeps {..})
        (viewportCenter .-^ (V2 0 32))
        "Start"
    --
    exitButton :: SimpleButton
    exitButton =
      simpleButton
        (SBDeps {..})
        (viewportCenter .+^ (V2 0 32))
        "Exit"
    --
    drawCallback _ _ text _ _ = do
      text $ \draw -> do
        void $ draw (startButton ^. #cmd)
        void $ draw (exitButton ^. #cmd)
    --
    soundCommands = V.empty
    intent = Nothing
    loadRequests = empty
    (exit, start) =
      let Input.Full {..} = fullInput
      in (
        quitEvent || keyUp Input.Key'Escape || exitButton ^. #clicked,
        keyUp Input.Key'Enter || startButton ^. #clicked
      )
  when exit $ dispatch Root.Action'ExitScreen
  when start $ dispatch Root.Action'StartBattle
  return (Root.ScreenOut {..})
