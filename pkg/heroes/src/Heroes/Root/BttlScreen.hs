module Heroes.Root.BttlScreen (
  Deps(..),
  Prov(..),
  In(..),
  Data,
  with
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation                                         (GroupSizeOf)
import Animation.Scene                                   (Actor)
import Animation.Scene                                   (Handle(..))
import Animation.Scene                                   (Prop)
import Animation.Scene                                   (Scene(..))
import Battle                                            (Battle)
import Battle                                            (FighterId)
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
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data In = In {
  dispatch :: Root.Action -> IO (),
  fullInput :: Input.Full,
  loaded :: Loaded
} deriving (Generic)

data Deps = Deps {
  queryAI :: IO (Maybe AIResult),
  askAI :: Maybe AIQuery -> IO (),
  groupSizeOf :: GroupSizeOf,
  setup :: Setup,
  initialBattle :: Battle
} deriving (Generic)

data Prov = Prov {
  new :: IO Data,
  run :: In -> Data -> IO Root.Out
} deriving (Generic)

data Data = Data {
  core :: IORef Core.Data,
  blackbox :: IORef Blackbox.Data,
  animation :: IORef Animation.Data,
  groupSizeOf :: GroupSizeOf,
  initialBattle :: Battle,
  setup :: Setup,
  queryAI :: IO (Maybe AIResult),
  askAI :: Maybe AIQuery -> IO ()
} deriving (Generic)

with :: Deps -> With Prov
with deps next = next (Prov {
  new = new' deps,
  run = run'
})

new' :: Deps -> IO Data
new' (Deps {..}) = do
  core <- newIORef (Core.Data {
    current = Current (setup, initialBattle),
    pastBattles = [],
    futureBattles = []
  })
  animation <- newIORef (Scene {
    actors = empty,
    props = empty,
    curtain = 1.0
  })
  blackbox <- newIORef (Blackbox.Data {
    updateOrPlan = Left . AM.JumpTo . Some $ initialBattle,
    frameNumber = 0,
    subframeNumber = 0
  })
  return (Data {..})

run' :: In -> Data -> IO Root.Out
run' (In {..}) (Data {..}) = do
  Blackbox.Out {..} <- Blackbox.run (Core.run core) blackbox (Blackbox.In {..})
  when exit $ dispatch Root.Action'ExitScreen
  scene0 <- readIORef animation
  let
    scene1 = Animation.run (Animation.In {..}) scene0
    drawCallback = mkDrawCallback (DrawIn { scene = scene1, .. })
  writeIORef animation scene1
  return (Root.Out {..})
  
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
