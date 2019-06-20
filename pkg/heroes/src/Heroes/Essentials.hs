module Heroes.Essentials (
  get,
  put,
  Essentials(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Heroes.Common
import Heroes.Font                                       (Font)
import Heroes.FontMeta                                   (FontMeta)
import Heroes.SpriteMeta                                 (SpriteMeta)
import qualified Heroes.FontMeta                           as FontMeta
import qualified Heroes.SpriteMeta                         as SpriteMeta
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Binary.Get                                   (Get)
import Data.Binary.Put                                   (Put)
import qualified Data.Map.Strict                           as M
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Essentials = Essentials {
  fontMeta :: Font -> FontMeta,
  creatureMeta :: Creature -> SpriteMeta,
  sfxMeta :: SFX -> SpriteMeta
}

put :: Essentials -> Put
put (Essentials {..}) = do
  for_ genum (SpriteMeta.put . creatureMeta)
  for_ genum (SpriteMeta.put . sfxMeta)
  for_ genum (FontMeta.put . fontMeta)

get :: Get Essentials
get = do
  cs <- M.fromList <$> (for genum $ \c -> (c,) <$> SpriteMeta.get)
  ss <- M.fromList <$> (for genum $ \s -> (s,) <$> SpriteMeta.get)
  fs <- M.fromList <$> (for genum $ \f -> (f,) <$> FontMeta.get)
  let
    creatureMeta = (cs !)
    sfxMeta = (ss !)
    fontMeta = (fs !)
  --
  return $ Essentials {..}
