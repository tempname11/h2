{-# LANGUAGE RankNTypes #-}
module Heroes.Root (
  new,
  Root,
  Deps(..),
  module Heroes.Root.Common,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Essentials                                 (Essentials)
import Heroes.AAI                                        (AIQuery(..))
import Heroes.AAI                                        (AIResult(..))
import Heroes.Root.Common
import Stage.Loading                                     (Loaded)
import qualified Heroes.Input                              as Input
import qualified Heroes.GFX                                as GFX
import qualified Heroes.Root.BttlScreen                    as Bttl
import qualified Heroes.Root.MenuScreen                    as Menu
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Reflex.Jumpstart                          as J
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Root
  = Root'MenuScreen {
    out'E :: J.E Out,
    action'E :: J.E Action
  }
  | Root'BttlScreen {
    out'E :: J.E Out,
    action'E :: J.E Action
  }
  deriving (Generic)

data Deps = Deps {
  queryAI :: IO (Maybe AIResult),
  askAI :: Maybe AIQuery -> IO (),
  essentials :: Essentials,
  staticResources :: GFX.StaticResources
} deriving (Generic)

new :: (WSC, J.Sample m, J.Hold m, Monad m) =>
  Deps ->
  J.E Input.Full ->
  J.B Loaded ->
  m (J.E Out, J.B Bool)
new (Deps {..}) in'E loaded'B = do
  let
    run :: Action -> Root -> J.E Root
    run action root = do
      case action of
        Action'ExitScreen ->
          case root of
            Root'MenuScreen {} -> return root
            Root'BttlScreen {} -> do
              (out'E, action'E) <- Menu.new (Menu.Deps {..}) in'E
              return $ Root'MenuScreen {..}
        Action'StartBattle {..} -> do
          case root of
            Root'BttlScreen {} -> return root
            Root'MenuScreen {} -> do
              (out'E, action'E) <-
                Bttl.new (Bttl.Deps {..}) loaded'B in'E
              --
              return $ Root'BttlScreen {..}
  --
  initial <- do
    (o, a) <- Menu.new (Menu.Deps {..}) in'E
    return $ Root'MenuScreen o a
  --
  out'E <- do
    (_, o) <- J.holdFix initial $ \b ->
      let
        a = b <&> view #action'E
        o = b <&> view #out'E
        r = do
          action <- join $ J.sample a
          root <- J.sample b
          run action root
      --
      in (r, o)
    --
    return $ join $ J.sample o
  --
  exit'B <- J.hold False $ do
    Out {..} <- out'E
    return exit
  --
  return (out'E, exit'B)
