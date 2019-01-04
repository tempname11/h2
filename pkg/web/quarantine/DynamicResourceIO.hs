module Web.DynamicResourceIO where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import qualified Stage.Links                               as L
import qualified Web.Stage.Links                           as L
import qualified Web.ComplexSprite                         as ComplexSprite
import Web.CreatureResource                              (CreatureResource(..))
import Heroes.Essentials                                 (Essentials(..))
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  essentials :: L.Essentials,
  theContext :: L.TheContext
}
--------------------------------------------------------------------------------
loadCreature :: Deps -> Creature -> IO CreatureResource
loadCreature (Deps {..}) c = do
  let Essentials { creatureMeta } = essentials
      meta = creatureMeta c
  sprite <- ComplexSprite.load theContext c meta
  return $ CreatureResource sprite empty
--------------------------------------------------------------------------------
destroyCreature :: CreatureResource -> IO ()
destroyCreature _c = do
  return ()
