module Heroes.GFX (
  module Heroes.GFX,
  module Heroes.GFX'Types,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation.Scene                                   (Scene)
import Battle
import Heroes
import Heroes.Drawing                                    (FontAtlas)
import Heroes.Essentials                                 (Essentials)
import Heroes.Font                                       (Font)
import Heroes.GFX'Types
import Heroes.H3.Misc                                    (ObstacleType)
import Heroes.SpriteMeta                                 (SpriteMeta)
import Heroes.UI
import qualified Heroes.WND                                as WND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data In = In {
  darkHexes :: V.Vector Hex,
  extraColor :: FighterId -> Maybe Color,
  lightHexes :: V.Vector Hex,
  scene :: Scene
}

data Deps = Deps {
  essentials :: Essentials,
  window :: WND.Window
}

data Prov = Prov {
  staticResources :: StaticResources,
  renderer :: Renderer,
  draw :: Handler In
}

data StaticResources = StaticResources {
  cellShaded :: StaticSprite,
  cellOutline :: StaticSprite,
  background :: StaticSprite,
  obstacles :: ObstacleType -> StaticSprite,
  fonts :: Map Font FontAtlas
} deriving (Generic)

class GFX'Types => GFX where
  type Renderer
  loadComplexSprite :: Renderer -> SpriteMeta -> String -> IO ComplexSprite
  destroyComplexSprite :: ComplexSprite -> IO ()
  --
  with :: Deps -> With Prov
