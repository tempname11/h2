module Heroes.Essentials (
  get,
  put,
  groupSizeOf,
  Essentials(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation
import Common
import Heroes.Handle
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
import qualified Data.Vector                               as V
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

groupSizeOf :: Essentials -> Handle -> GroupNumber -> GroupSize
groupSizeOf (Essentials {..}) h g =
  case h of
    Handle'Fighter fyr ->
      groupSize (creatureMeta (fyr ^. _creature) ^. #groups) g
    Handle'SFX sfx -> groupSize (sfxMeta sfx ^. #groups) g

groupSize :: V.Vector (V.Vector a) -> GroupNumber -> GroupSize
groupSize gs (GroupNumber g) =
  GroupSize $
    case gs V.!? g of
      Just x -> V.length x
      Nothing -> 0
