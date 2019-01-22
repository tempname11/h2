module Heroes.Subsystems.GFX where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation.Scene                                   (Scene)
import Battle
import Heroes
import Heroes.StaticResources                            (StaticResources)
import Heroes.UI
import Stage.Loading                                     (Loaded)
import qualified Heroes.Platform                           as Platform
import qualified Heroes.Subsystems.WND                     as WND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data In = In {
  darkHexes :: [Hex],
  extraColor :: FighterId -> Maybe Color,
  lightHexes :: [Hex],
  loaded :: Loaded,
  scene :: Scene
}

data Deps = Deps {
  window :: WND.Window
}

data Prov = Prov {
  staticResources :: StaticResources,
  renderer :: Platform.Renderer
}

class GFX where
  with :: Deps -> With (Handler In, Prov)
