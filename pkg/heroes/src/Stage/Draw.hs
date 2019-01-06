module Stage.Draw where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation.Scene                                   (Scene)
import Battle
import Heroes
import Heroes.StaticResources                            (StaticResources)
import Heroes.UI
import Stage.Loading                                     (Loaded)
import qualified Heroes.Platform                           as Platform
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  renderer :: Platform.Renderer,
  staticResources :: StaticResources
}

data In = In {
  darkHexes :: [Hex],
  extraColor :: FighterId -> Maybe Color,
  lightHexes :: [Hex],
  loaded :: Loaded,
  scene :: Scene
}

class Draw where
  with :: Deps -> ((In -> IO ()) -> IO a) -> IO a
