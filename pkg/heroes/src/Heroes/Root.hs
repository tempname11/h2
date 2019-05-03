module Heroes.Root (
  module Heroes.Root,
  module Heroes.Root.Common,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation                                         (GroupSizeOf)
import Battle                                            (Battle)
import Battle.Setup                                      (Setup)
import Heroes
import Heroes.AAI                                        (AIQuery(..))
import Heroes.AAI                                        (AIResult(..))
import Heroes.Root.Common
import qualified Heroes.Root.BattleScreen                  as BattleScreen
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
  titleScreen <- TitleScreen.init
  return $ Root'TitleScreen titleScreen

action :: Deps -> Action -> Root -> IO (Maybe Root)
action (Deps {..}) a r =
  case a of
    Action'ExitScreen ->
      case r of
        Root'TitleScreen {} -> return Nothing
        Root'BattleScreen {} -> do
          return (Just $ Root'TitleScreen TitleScreen.Data {})
    Action'StartBattle -> do
      battleScreen <- BattleScreen.init (BattleScreen.Deps {..})
      return (Just $ Root'BattleScreen battleScreen)
