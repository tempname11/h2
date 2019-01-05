{-# LANGUAGE TemplateHaskell #-}
module Native.Resource where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.H3.Misc                                    (ObstacleType)
import Heroes.Platform                                   (Chunk)
import Heroes.Platform                                   (ComplexSprite)
import Heroes.Platform                                   (StaticSprite)
import qualified Heroes.UI.Sound                           as Sound
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Loaded = Loaded {
  creatures :: Creature -> Maybe CreatureResource,
  sfxes :: SFX -> Maybe SFXResource
}

data CreatureResource = CreatureResource {
  sprite :: ComplexSprite,
  sounds :: Map Sound.CType Chunk
}

data SFXResource = SFXResource {
  sprite :: ComplexSprite,
  sound :: Chunk
}

data StaticResources = StaticResources {
  cellShaded :: StaticSprite,
  cellOutline :: StaticSprite,
  background :: StaticSprite,
  obstacles :: ObstacleType -> StaticSprite
}

makeShorthands ''CreatureResource
makeShorthands ''Loaded
makeShorthands ''SFXResource
makeShorthands ''StaticResources
