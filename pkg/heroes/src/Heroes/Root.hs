{-# LANGUAGE RecursiveDo #-}
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

new :: (WSC, J.Network m) => Deps -> J.E Input.Full -> J.B Loaded -> m (J.E Out, J.B Bool)
new (Deps {..}) in'E loaded'B = mdo
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
  out''E <- mdo
    b <- J.hold initial r
    a <- J.sample (b <&> view #action'E)
    o <- J.sample (b <&> view #out'E)
    let
      r =  do
        action <- a
        root <- J.sample b
        run action root
    --
    return o
  --
  exit'B <- J.hold False $ out''E <&> \(Out {..}) -> exit
  return (out''E, exit'B)
