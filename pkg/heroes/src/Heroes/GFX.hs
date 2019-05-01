module Heroes.GFX where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Drawing                                    (ComplexSprite)
import Heroes.Drawing                                    (FontAtlas)
import Heroes.Essentials                                 (Essentials)
import Heroes.Drawing                                    (StaticSprite)
import Heroes.Font                                       (Font)
import Heroes.H3.Misc                                    (ObstacleType)
import Heroes.SpriteMeta                                 (SpriteMeta)
import qualified Heroes.Drawing.OneColor                   as OneColor
import qualified Heroes.Drawing.Paletted                   as Paletted
import qualified Heroes.Drawing.Text                       as Text
import qualified Heroes.Drawing.Regular                    as Regular
import qualified Heroes.WND                                as WND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type DrawCallback =
  With (Handler Regular.Cmd) ->
  With (Handler Paletted.Cmd) ->
  With (Handler Text.Cmd) ->
  With (Handler OneColor.Cmd) ->
  StaticResources ->
  IO ()

data Deps = Deps {
  essentials :: Essentials,
  window :: WND.Window
}

data Prov = Prov {
  staticResources :: StaticResources,
  renderer :: Renderer,
  draw :: Handler DrawCallback
}

data StaticResources = StaticResources {
  cellShaded :: StaticSprite,
  cellOutline :: StaticSprite,
  background :: StaticSprite,
  obstacles :: ObstacleType -> StaticSprite,
  fonts :: Map Font FontAtlas
} deriving (Generic)

class GFX where
  type Renderer
  loadComplexSprite :: Renderer -> SpriteMeta -> String -> IO ComplexSprite
  destroyComplexSprite :: ComplexSprite -> IO ()
  --
  with :: Deps -> With Prov
