module Heroes.Root.MenuScreen (
  Deps(..),
  Prov(..),
  with
) where

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
import qualified Data.Unique                               as U
import qualified Data.Vector                               as V
import qualified Reflex.Jumpstart                          as J
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  staticResources :: GFX.StaticResources
} deriving (Generic)

type New =
  J.B U.Unique ->
  J.E Input.Full ->
  J.Runtime (U.Unique, J.E Root.Out, J.E Root.Action)

data Prov = Prov {
  new :: New 
} deriving (Generic)

data Self
  = Self'Title
  | Self'Lobby
  deriving (Generic)

with :: Deps -> With Prov
with deps next = next (Prov { new = new' deps })

emptyOut :: Root.Out
emptyOut = Root.Out {
  drawCallback = \_ _ _ _ _ -> return (),
  soundCommands = V.empty,
  intent = Nothing,
  loadRequests = empty,
  exit = False
}

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

new' :: Deps -> New
new' (Deps {..}) unique'B in'E = do
  u <- liftIO $ U.newUnique
  let in''E = J.gate (unique'B <&> \u' -> u == u') in'E
  -- let self'B = return Self'Title
  let
    multi'E =
      in''E <&> \fullInput -> do
        let
          Input.Full {..} = fullInput
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
          start =
            keyUp Input.Key'Enter ||
            startButton ^. #clicked
          --
          exit =
            quitEvent ||
            keyUp Input.Key'Escape ||
            exitButton ^. #clicked
          --
          out = emptyOut
            & #drawCallback .~ drawCallback
            & #exit .~ exit
        --
        (out, start)
    --
    out'E = multi'E <&> view _1
    start'E = multi'E <&> view _2
    action'E = J.justE $ start'E <&> \case
      True -> Just Root.Action'StartBattle
      False -> Nothing
  --
  return (u, out'E, action'E)
