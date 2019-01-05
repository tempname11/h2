{-# LANGUAGE TemplateHaskell #-}
module Heroes.CreatureResource where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Platform                                   (Chunk)
import Heroes.Platform                                   (ComplexSprite)
import qualified Heroes.UI.Sound                           as Sound
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data CreatureResource = CreatureResource {
  sprite :: ComplexSprite,
  sounds :: Map Sound.CType Chunk
}

makeShorthands ''CreatureResource
