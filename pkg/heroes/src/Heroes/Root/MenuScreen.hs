module Heroes.Root.MenuScreen (
  Deps(..),
  new
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Protocol'Lobby
import Heroes.Root.MenuScreen.Client
import Heroes.Font                                       (Font(..))
import Heroes.UI                                         (viewportSize)
import qualified Heroes.Color                              as Color
import qualified Heroes.Input                              as Input
import qualified Heroes.Drawing.Text                       as Text
import qualified Heroes.GFX                                as GFX
import qualified Heroes.Root.Common                        as Root
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Text.Encoding                                (encodeUtf8)
import qualified Data.Text                                 as T
import qualified Data.Vector                               as V
import qualified Reflex.Jumpstart                          as J
import qualified Control.Concurrent.Async                  as Async
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  staticResources :: GFX.StaticResources
} deriving (Generic)

data Self
  = Self'Title
  | Self'Lobby (Async [ID "Match"])
  deriving (Generic)

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
  pressed :: Bool,
  cmd :: Text.Cmd
} deriving (Generic)

simpleButton :: SBDeps -> Point V2 Float -> Text -> SimpleButton
simpleButton (SBDeps {..}) center string = SimpleButton {..}
  where
  Input.Full {..} = fullInput
  GFX.StaticResources {..} = staticResources
  (box, preparation) =
    Text.prepare
      (fonts ! Font'CompassPro24)
      (encodeUtf8 string)
  screenPlace = center .-^ box * 0.5
  hovered = inBounds screenPlace box (fmap2 (§) mouseAt)
  pressed = hovered && mouseDown Input.LMB
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

gate :: Bool -> J.E ()
gate = \case
  False -> mempty
  True -> return ()
  
new :: (WSC, J.Network m) => Deps -> J.E Input.Full -> m (J.E Root.Out, J.E Root.Action)
new (Deps {..}) in'E = do
  (_, result) <- J.holdFix Self'Title $ \self'B ->
    let
      multi'E = do
        fullInput <- in'E
        self <- J.sample self'B
        case self of
          Self'Title -> do
            let
              Input.Full {..} = fullInput
              --
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
              start = gate $
                keyUp Input.Key'Enter ||
                startButton ^. #pressed
              --
              exit = -- gate $
                quitEvent ||
                keyUp Input.Key'Escape ||
                exitButton ^. #pressed
              --
              out = emptyOut
                & #drawCallback .~ drawCallback
                & #exit .~ exit
            --
            return (out, start, mempty)
          Self'Lobby lm -> do
            p <- J.affect $ Async.poll lm
            let
              str :: Text
              str = case p of
                Nothing -> "Nothing"
                Just (Left _) -> "Exception"
                Just (Right ms) -> T.intercalate " " $
                  ms <&> T.pack . show
              --
              Input.Full {..} = fullInput
              --
              createButton :: SimpleButton
              createButton =
                simpleButton
                  (SBDeps {..})
                  (viewportCenter .-^ (V2 0 64))
                  "Create"
              --
              itemButton :: SimpleButton
              itemButton =
                simpleButton
                  (SBDeps {..})
                  viewportCenter
                  str
              --
              backButton :: SimpleButton
              backButton =
                simpleButton
                  (SBDeps {..})
                  (viewportCenter .+^ (V2 0 64))
                  "Back"
              --
              {-
              create =
                keyUp Input.Key'Enter ||
                createButton ^. #pressed
              -}
              --
              back = gate $
                keyUp Input.Key'Escape ||
                backButton ^. #pressed
              --
              drawCallback _ _ text _ _ = do
                text $ \draw -> do
                  void $ draw (createButton ^. #cmd)
                  void $ draw (itemButton ^. #cmd)
                  void $ draw (backButton ^. #cmd)
              --
              out = emptyOut
                & #drawCallback .~ drawCallback
            --
            return (out, mempty, back)
      --
      out'E = multi'E <&> view _1
      start'E = join $ multi'E <&> view _2
      back'E = join $ multi'E <&> view _3
      toLobby'E = do
        start'E
        lm <- J.affect $ listMatches
        return $ Self'Lobby lm
      --
      toTitle'E = back'E <&> \_ -> Self'Title
      self'E = toLobby'E <> toTitle'E
      action'E = mempty
      --
    in (self'E, (out'E, action'E))
  --
  return result
