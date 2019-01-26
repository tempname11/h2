{-# LANGUAGE TemplateHaskell #-}
module Heroes.GFX (
  module Heroes.GFX,
  module Heroes.GFX'Types,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation.Scene                                   (Scene)
import Battle
import Heroes
import Heroes.GFX'Types
import Heroes.H3.Misc                                    (ObstacleType)
import Heroes.SpriteMeta                                 (Meta)
import Heroes.UI
import qualified Heroes.WND                                as WND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data In = In {
  darkHexes :: [Hex],
  extraColor :: FighterId -> Maybe Color,
  lightHexes :: [Hex],
  scene :: Scene
}

data Deps = Deps {
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
  obstacles :: ObstacleType -> StaticSprite
}

class GFX'Types => GFX where
  type Renderer
  loadComplexSprite :: Renderer -> Meta -> String -> IO ComplexSprite
  destroyComplexSprite :: ComplexSprite -> IO ()
  --
  with :: Deps -> With Prov

makeShorthands ''StaticResources
