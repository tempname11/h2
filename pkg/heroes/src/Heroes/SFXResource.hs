{-# LANGUAGE TemplateHaskell #-}
module Heroes.SFXResource where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Platform                                   (Chunk)
import Heroes.Platform                                   (ComplexSprite)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data SFXResource = SFXResource {
  sprite :: ComplexSprite,
  sound :: Chunk
}

makeShorthands ''SFXResource
