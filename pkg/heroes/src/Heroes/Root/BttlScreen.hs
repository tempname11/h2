module Heroes.Root.BttlScreen (
  Deps(..),
  new,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation.Scene                                   (Actor)
import Animation.Scene                                   (Prop)
import Animation.Scene                                   (Scene(..))
import Battle                                            (Battle)
import Battle                                            (ObstacleId)
import Battle                                            (_otype)
import qualified Battle.AM                                 as AM
import Battle.Setup                                      (Setup)
import Heroes
import Heroes.AAI                                        (AIQuery)
import Heroes.AAI                                        (AIResult)
import Heroes.Drawing                                    (CopySpec(..))
import Heroes.Color                                      (Color)
import Heroes.Color                                      (transparent)
import Heroes.UI                                         (fieldCenter)
import Heroes.Essentials                                 (Essentials)
import Stage.Loading                                     (Loaded)
import qualified Heroes.Cell                               as Cell
import qualified Heroes.Drawing                            as Drawing
import qualified Heroes.Drawing.OneColor                   as OneColor
import qualified Heroes.Drawing.Paletted                   as Paletted
import qualified Heroes.Drawing.Regular                    as Regular
import qualified Heroes.Input                              as Input
import qualified Heroes.GFX                                as GFX
import qualified Heroes.Root.Common                        as Root
import qualified Heroes.Root.BttlScreen.Animation          as Animation
import qualified Heroes.Root.BttlScreen.Blackbox           as Blackbox 
import qualified Heroes.Root.BttlScreen.Core               as Core 
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
import qualified Data.Vector                               as V
import qualified Data.Vector.Generic                       as GV
import qualified Data.Vector.Storable                      as SV
import qualified Reflex.Jumpstart                          as J
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  queryAI :: IO (Maybe AIResult),
  askAI :: Maybe AIQuery -> IO (),
  essentials :: Essentials,
  setup :: Setup,
  initialBattle :: Battle
} deriving (Generic)

type New =
  J.B Loaded ->
  J.E Input.Full ->
  J.E (J.E Root.Out, J.E Root.Action)

new :: Deps -> New
new (Deps {..}) loaded'B in'E = do
  -- TODO Behavior!
  core <- J.affect $ newIORef (Core.Data {
    current = Current (setup, initialBattle),
    pastBattles = [],
    futureBattles = []
  })
  -- TODO Behavior!
  animation <- J.affect $ newIORef (Scene {
    actors = vacant,
    props = vacant,
    curtain = 1.0
  })
  -- TODO Behavior!
  blackbox <- J.affect $ newIORef (Blackbox.Data {
    updateOrPlan = Left . AM.JumpTo $ initialBattle,
    frameNumber = 0,
    subframeNumber = 0
  })
  let
    multi'E = do
      fullInput <- in'E
      loaded <- J.sample loaded'B
      J.affect $ do
        Blackbox.Out {..} <- Blackbox.run
          (Core.run core)
          blackbox
          (Blackbox.In {..})
        --
        scene0 <- readIORef animation
        let
          scene1 = Animation.run (Animation.In {..}) scene0
          drawCallback = mkDrawCallback (DrawIn { scene = scene1, .. })
        writeIORef animation scene1
        return (Root.Out { exit = False {- TODO -}, .. }, exit)
    out'E = multi'E <&> view _1
    exit'E = multi'E <&> view _2
    action'E = do
      exit'E >>= \case
        True -> return Root.Action'ExitScreen
        False -> empty
  --
  return (out'E, action'E)
  
data DrawIn = DrawIn {
  darkHexes :: V.Vector Hex,
  extraColor :: FighterId -> Maybe Color,
  lightHexes :: V.Vector Hex,
  scene :: Scene
} deriving (Generic)

mkDrawCallback :: DrawIn -> GFX.DrawCallback
mkDrawCallback (DrawIn {..}) regular paletted _ oneColor staticResources = do
  let
    comparingY = comparing (view $ _2 . #position . _y)
    actors = sortBy comparingY $ M.assocs $ scene ^. #actors
    props = M.assocs $ scene ^. #props
    GFX.StaticResources {..} = staticResources
    bgCmd = background `fullCopyAt` (SV.singleton 0)
    --
    regularCmds =
      [bgCmd] <>
      (fromProp <$> props) <>
      [
        hexCmd cellShaded darkHexes,
        hexCmd cellOutline lightHexes
      ]
    --
    hexCmd :: Drawing.StaticSprite -> V.Vector Hex -> Regular.Cmd
    hexCmd sprite hexes =
      sprite `fullCopyAt`
        GV.convert ((\hex -> (<§>) (fieldCenter .+^ Cell.fromHex hex)) `GV.map` hexes)
    --
    fromProp :: (ObstacleId, Prop) -> Regular.Cmd
    fromProp (o, prop) = cmd
      where
      sprite = obstacles (o ^. _otype)
      sign = case prop ^. #facing of
        West -> V2 (-1) 1
        East -> 1
      screenPlace = (<§>) (prop ^. #position)
      cmd = Regular.Cmd {
        texture = sprite ^. #texture,
        dimensions = sprite ^. #dimensions,
        box = sprite ^. #dimensions,
        place = 0,
        screenPlaces = SV.singleton screenPlace,
        screenBox = (sprite ^. #dimensions) * sign
      }
    --
    palettedCmds = fromActor extraColor <$> actors
  --
  regular $ \draw ->
    for_ regularCmds draw
  --
  paletted $ \draw ->
    for_ palettedCmds draw
  --
  oneColor $ \draw -> do
    let color = V4 0 0 0 (floor . (255 *) $ (scene ^. #curtain))
    draw $ OneColor.Cmd { color, box = Nothing, place = 0 }

fromActor ::
  (FighterId -> Maybe Color) ->
  (Handle, Actor) ->
  Paletted.Cmd
fromActor extraColor (h, actor) = Paletted.Cmd sprite spec outlineColor
  where
  outlineColor =
    case h of
      Handle'Fighter fyr -> maybe transparent id (extraColor fyr)
      _ -> transparent
    
  sprite = actor ^. #sprite . _some
  frame = (sprite ^. #meta . #groups) & (! g) & (! f) -- XXX partial...
  f = actor ^. #frameN
  g = actor ^. #groupN
  -- @copypaste from Native.Stage.Prepare.toCopy
  facing = actor ^. #facing
  screenPlace = (<§>) ((actor ^. #position) .+^ offset .-^ (V2 0 (actor ^. #height)))
  offset = (frame ^. #offset) * sign
  sign = case facing of
    West -> V2 (-1) 1
    East -> 1
  spec = CopySpec {
    box = (<§>) (frame ^. #box),
    place = (<§>) (frame ^. #place),
    screenPlace,
    screenBox = (<§>) ((frame ^. #box) * sign)
  }

fullCopyAt :: Drawing.StaticSprite -> SV.Vector (Point V2 Float) -> Regular.Cmd
fullCopyAt sprite screenPlaces =
  Regular.Cmd {
    texture = sprite ^. #texture,
    dimensions = sprite ^. #dimensions,
    box = sprite ^. #dimensions,
    screenBox = sprite ^. #dimensions,
    place = 0,
    screenPlaces
  }
