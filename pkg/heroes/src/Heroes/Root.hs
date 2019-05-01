module Heroes.Root (
  module Heroes.Root,
  module Heroes.Root.Common,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation                                         (GroupSizeOf)
import Animation.Scene                                   (Scene(..))
import Battle                                            (Battle)
import Battle.Setup                                      (Setup)
import Heroes
import Heroes.AAI                                        (AIQuery(..))
import Heroes.AAI                                        (AIResult(..))
import Heroes.Root.Common
import qualified Battle.AM                                 as AM
import qualified Heroes.Root.BattleScreen                  as BattleScreen
import qualified Heroes.Root.BattleScreen.Blackbox         as Blackbox 
import qualified Heroes.Root.BattleScreen.Core             as Core 
import qualified Heroes.Root.TitleScreen                   as TitleScreen
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Root
  = Root'TitleScreen TitleScreen.Data
  | Root'BattleScreen BattleScreen.Data
  deriving (Generic)

data Deps = Deps {
  queryAI :: IO (Maybe AIResult),
  askAI :: Maybe AIQuery -> IO (),
  groupSizeOf :: GroupSizeOf,
  setup :: Setup,
  initialBattle :: Battle
} deriving (Generic)

init :: Deps -> IO Root
init (Deps {..}) = do
  core <- newIORef (Core.Data {
    current = Current (setup, initialBattle),
    pastBattles = [],
    futureBattles = []
  })
  animation <- newIORef (Scene {
    actors = empty,
    props = empty,
    curtain = 1.0
  })
  blackbox <- newIORef (Blackbox.Data {
    updateOrPlan = Left . AM.JumpTo . Some $ initialBattle,
    frameNumber = 0,
    subframeNumber = 0
  })
  return $ Root'BattleScreen BattleScreen.Data {..}
