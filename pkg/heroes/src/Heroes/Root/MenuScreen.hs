module Heroes.Root.MenuScreen (
  Deps(..),
  Prov(..),
  with
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
import qualified Data.Unique                               as U
import qualified Data.Vector                               as V
import qualified Reflex.Jumpstart                          as J
import qualified Control.Concurrent.Async                  as Async
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
  | Self'Lobby (Async [ID "Match"])
  deriving (Generic)

with :: (WSC) => Deps -> With Prov
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

boolE :: J.E Bool -> J.E ()
boolE e = J.justE $ e <&> \case
  True -> Just ()
  False -> Nothing

new' :: (WSC) => Deps -> New
new' (Deps {..}) unique'B in'E = do
  u <- liftIO $ U.newUnique
  let in''E = J.gate (unique'B <&> \u' -> u == u') in'E
  fmap snd $ J.fixE $ \self'E0 -> do
    self'B <- J.hold Self'Title self'E0
    multi'E <- J.subscribe u in''E $ \fullInput -> do
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
            start =
              keyUp Input.Key'Enter ||
              startButton ^. #pressed
            --
            exit =
              quitEvent ||
              keyUp Input.Key'Escape ||
              exitButton ^. #pressed
            --
            out = emptyOut
              & #drawCallback .~ drawCallback
              & #exit .~ exit
          --
          return (out, start, Nothing, False)
        Self'Lobby lm -> do
          p <- liftIO $ Async.poll lm
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
            create = Nothing
            {-
              keyUp Input.Key'Enter ||
              createButton ^. #pressed
            -}
            --
            back =
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
          return (out, False, create, back)
    --
    let
      out'E = multi'E <&> view _1
      start'E = boolE $ multi'E <&> view _2
      create'E = J.justE $ multi'E <&> view _3
      back'E = boolE $ multi'E <&> view _4
    --
    toLobby'E <- J.subscribe () start'E $ \_ -> do
      lm <- liftIO $ listMatches
      return $ Self'Lobby lm
    --
    let
      toTitle'E = back'E <&> \_ -> Self'Title
      self'E = toLobby'E <> toTitle'E
      action'E = create'E
    --
    return (self'E, (u, out'E, action'E))
