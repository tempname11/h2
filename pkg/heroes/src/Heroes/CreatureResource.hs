module Heroes.CreatureResource where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Essentials                                 (Essentials(..))
import Heroes.Platform                                   (Platform)
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.H3                                 as H3
import qualified Heroes.GFX                                as GFX
import qualified Heroes.SND                                as SND
import qualified Heroes.Sound                              as Sound
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data CreatureResource = CreatureResource {
  sprite :: GFX.ComplexSprite,
  sounds :: Map Sound.CType SND.Chunk
} deriving (Generic)

--------------------------------------------------------------------------------

load ::
  (GFX.GFX, SND.SND, Platform) =>
  GFX.Renderer ->
  Essentials ->
  Creature ->
  IO CreatureResource
load r (Essentials {..}) c = do
  let
    pngPath = FilePath.pngPathOf (H3.cDefName c)
    meta = creatureMeta c
  --
  sprite <- GFX.loadComplexSprite r meta pngPath
  --
  let
    soundTypes =
      Sound.allTypes &
        if H3.shoots c
          then id
          else filter (/= Sound.Shot)
  --
  sounds <- fmap M.fromList $ for soundTypes $ \t -> do
    let path = FilePath.soundPathOf (dir <> prefix <> suffix)
        dir = "battle-creatures/"
        prefix = H3.cSndName c
        suffix = Sound.suffix t
    putStrLn $ "Loading sound... " <> path
    (,) t <$> SND.loadChunk path
  --
  return $ CreatureResource { sprite, sounds }

destroy :: (GFX.GFX, SND.SND) => CreatureResource -> IO ()
destroy c = do
  GFX.destroyComplexSprite (c ^. #sprite)
  mapM_ SND.freeChunk (c ^. #sounds)
