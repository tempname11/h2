module Heroes.Essentials (
  getIt,
  putIt,
  Essentials(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import qualified Heroes.SpriteMeta                         as Meta
import Heroes.SpriteMeta                                 (Meta)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
import Prelude                                           (fromEnum)
import Data.Binary.Put                                   (Put)
import Data.Binary.Get                                   (Get)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Essentials = Essentials {
  creatureMeta :: Creature -> Meta,
  sfxMeta :: SFX -> Meta
}

putIt :: Essentials -> Put
putIt (Essentials {..}) = do
  for_ allCreatures (Meta.putIt . creatureMeta)
  for_ allSfx (Meta.putIt . sfxMeta)

getIt :: Get Essentials
getIt = do
  creatureMetas <- for allCreatures (const Meta.getIt)
  sfxMetas <- for allSfx (const Meta.getIt)
  let creatureMeta creature = creatureMetas ! fromEnum creature
  let sfxMeta sfx = sfxMetas ! fromEnum sfx
  return $ Essentials {..}

allCreatures :: V.Vector Creature
allCreatures = V.fromList [minBound .. maxBound]

allSfx :: V.Vector SFX
allSfx = V.fromList [minBound .. maxBound]
