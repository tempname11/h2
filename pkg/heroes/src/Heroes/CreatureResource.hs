{-# LANGUAGE TemplateHaskell #-}
module Heroes.CreatureResource where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Essentials                                 (Essentials(..))
import Heroes.Platform                                   (Platform)
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.H3                                 as H3
import qualified Heroes.Platform                           as Platform
import qualified Heroes.GFX                                as GFX
import qualified Heroes.UI.Sound                           as Sound
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data CreatureResource = CreatureResource {
  sprite :: GFX.ComplexSprite,
  sounds :: Map Sound.CType Platform.Chunk
}

makeShorthands ''CreatureResource

--------------------------------------------------------------------------------

load ::
  (GFX.GFX, Platform) =>
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
  let soundTypes = Sound.allTypes & if H3.shoots c
                                    then id
                                    else filter (/= Sound.Shot)
  --
  sounds <- fmap M.fromList $ for soundTypes $ \t -> do
    let path = FilePath.prod <> "Sounds/" <> prefix <> suffix <> ".wav"
        prefix = H3.cSndName c
        suffix = Sound.suffix t
    putStrLn $ "Loading sound... " <> path
    (,) t <$> Platform.loadChunk path
  --
  return $ CreatureResource { sprite, sounds }

destroy :: (GFX.GFX, Platform) => CreatureResource -> IO ()
destroy c = do
  GFX.destroyComplexSprite (c ^. sprite_)
  mapM_ Platform.freeChunk (c ^. sounds_)
