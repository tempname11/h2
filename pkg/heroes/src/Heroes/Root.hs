module Heroes.Root (
  with,
  Root,
  Deps(..),
  Prov(..),
  In(..),
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
import Stage.Loading                                     (Loaded)
import qualified Heroes.Input                              as Input
import qualified Heroes.GFX                                as GFX
import qualified Heroes.Root.BttlScreen                    as BttlScreen
import qualified Heroes.Root.MenuScreen                    as MenuScreen
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Root
  = Root'MenuScreen MenuScreen.Data
  | Root'BttlScreen BttlScreen.Data
  deriving (Generic)

data Deps = Deps {
  queryAI :: IO (Maybe AIResult),
  askAI :: Maybe AIQuery -> IO (),
  groupSizeOf :: GroupSizeOf,
  setup :: Setup,
  initialBattle :: Battle,
  staticResources :: GFX.StaticResources
} deriving (Generic)

data Prov = Prov {
  new :: IO Root,
  run :: In -> Root -> IO Out,
  action :: Action -> Root -> IO (Maybe Root)
} deriving (Generic)

data Priv = Priv {
  menu :: MenuScreen.Prov,
  bttl :: BttlScreen.Prov
} deriving (Generic)

with :: Deps -> With Prov
with (Deps {..}) next =
  MenuScreen.with (MenuScreen.Deps {..}) $ \menu ->
  BttlScreen.with (BttlScreen.Deps {..}) $ \bttl ->
    let p = Priv {..}
    in next (Prov {
      new = new' p,
      run = run' p,
      action = action' p
    })

new' :: Priv -> IO Root
new' (Priv {..}) = Root'MenuScreen <$> (menu ^. #new)

data In = In {
  dispatch :: Action -> IO (),
  fullInput :: Input.Full,
  loaded :: Loaded
} deriving (Generic)

run' :: Priv -> In -> Root -> IO Out
run' (Priv {..}) (In {..}) = \case
  Root'MenuScreen s -> (menu ^. #run) (MenuScreen.In {..}) s
  Root'BttlScreen s -> (bttl ^. #run) (BttlScreen.In {..}) s

action' :: Priv -> Action -> Root -> IO (Maybe Root)
action' (Priv {..}) a r =
  case a of
    Action'ExitScreen ->
      case r of
        Root'MenuScreen {} -> return Nothing
        Root'BttlScreen {} -> do
          s <- (menu ^. #new)
          return (Just $ Root'MenuScreen s)
    Action'StartBattle -> do
      s <- (bttl ^. #new)
      return (Just $ Root'BttlScreen s)
