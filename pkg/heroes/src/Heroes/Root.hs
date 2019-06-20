module Heroes.Root (
  with,
  Root,
  Deps(..),
  Prov(..),
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
import qualified Data.Unique                               as U
import qualified Reflex.Jumpstart                          as J
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Root
  = Root'MenuScreen {
    unique :: U.Unique,
    out'E :: J.E Out,
    action'E :: J.E Action
  }
  | Root'BttlScreen {
    unique :: U.Unique,
    out'E :: J.E Out,
    action'E :: J.E Action
  }
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
  new :: J.E Input.Full -> J.B Loaded -> J.Runtime (J.E Out, J.B Bool)
} deriving (Generic)

with :: (WSC) => Deps -> With Prov
with (Deps {..}) next =
  MenuScreen.with (MenuScreen.Deps {..}) $ \menu ->
  BttlScreen.with (BttlScreen.Deps {..}) $ \bttl -> do
    let
      new in'E loaded'B = fmap snd $ J.fixB $ \unique'B0 -> do
        let
          run :: Action -> Root -> J.Runtime Root
          run action root = do
            case action of
              Action'ExitScreen ->
                case root of
                  Root'MenuScreen {} -> return root
                  Root'BttlScreen {} -> do
                    (unique, out'E, action'E) <-
                      (menu ^. #new) unique'B0 in'E
                    return $ Root'MenuScreen {..}
              Action'StartBattle -> do
                case root of
                  Root'BttlScreen {} -> return root
                  Root'MenuScreen {} -> do
                    (unique, out'E, action'E) <-
                      (bttl ^. #new) unique'B0 loaded'B in'E
                    return $ Root'BttlScreen {..}
        --
        initial <- do
          (u, o, a) <- (menu ^. #new) unique'B0 in'E
          return $ Root'MenuScreen u o a
        --
        (out'E, unique'B) <- fmap snd $ J.fixE $ \r0 -> do
          b <- J.hold initial r0
          let
            a = J.switch (b <&> view #action'E)
            o = J.switch (b <&> view #out'E)
            u = b <&> view #unique
          --
          r <- J.subscribe () a $ \action -> do
            root <- J.sample b
            run action root
          --
          return (r, (o, u))
        --
        exit'B <- J.hold False $ out'E <&> \(Out {..}) -> exit
        return (unique'B, (out'E, exit'B))
    --
    next (Prov {..})
