module Heroes.Root.MenuScreen (
  Data,
  Deps(..),
  Prov(..),
  In(..),
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
import Control.Monad.IO.Class                            (liftIO)
import qualified Data.Vector                               as V
import qualified Reflex.Jumpstart                          as J
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  staticResources :: GFX.StaticResources
} deriving (Generic)

data Prov = Prov {
  new :: IO Data,
  run :: In -> Data -> IO Root.Out
} deriving (Generic)

data In = In {
  fullInput :: Input.Full,
  dispatch :: Root.Action -> IO ()
} deriving (Generic)

data Data = Data {
  in'T :: J.Trigger In,
  out'B :: J.B Root.Out
} deriving (Generic)

data Self
  = Self'Title
  | Self'Lobby
  deriving (Generic)

with :: Deps -> With Prov
with deps next = do
  next (Prov {
    new = new' deps,
    run = run'
  })

emptyOut :: Root.Out
emptyOut = Root.Out {
  drawCallback = \_ _ _ _ _ -> return (),
  soundCommands = V.empty,
  intent = Nothing,
  loadRequests = empty
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

run' :: In -> Data -> IO Root.Out
run' in_ (Data { in'T, out'B }) = do
  J.fire [in'T J.==> in_]
  J.run $ J.sample out'B

new' :: Deps -> IO Data
new' (Deps {..}) = do
  (in'E, in'T) <- J.newEvent
  -- let self'B = return Self'Title
  let
    multi'E =
      in'E <&> \(In {..}) -> do
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
          out = emptyOut & #drawCallback .~ drawCallback
          exit =
            quitEvent ||
            keyUp Input.Key'Escape ||
            exitButton ^. #clicked
          start =
            keyUp Input.Key'Enter ||
            startButton ^. #clicked
          io = do
            when exit $ dispatch Root.Action'ExitScreen
            when start $ dispatch Root.Action'StartBattle
        --
        (out, io)
    --
    out'E = multi'E <&> view _1
    io'E = multi'E <&> view _2
  --
  J.run $ do
    _ <- J.subscribe io'E liftIO
    out'B <- J.hold emptyOut out'E
    return (Data {..})
