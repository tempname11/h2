{-# LANGUAGE RankNTypes #-}
module Stage.Blackbox (
  with,
  Deps (..),
  In (..),
  Out(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation                                         (GroupSizeOf)
import Battle                                            (Battle)
import Battle                                            (FighterId)
import Battle.Setup                                      (Setup)
import Heroes
import Heroes.Aux                                        (Annotation)
import Heroes.Plan                                       (Plan)
import Heroes.UI                                         (Color)
import Stage.Loading                                     (Loaded)
import Stage.LoadingThread                               (LoadRequest(..))
import qualified Animation.Command                         as Animation
import qualified Heroes.Input                              as Input
import qualified Heroes.Plan                               as Plan
import qualified Heroes.UI.Sound                           as Sound
import qualified Stage.Core                                as C
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  groupSizeOf :: GroupSizeOf,
  initialBattle :: Battle,
  setup :: Setup
}

data In = In {
  fullInput :: Input.Full,
  loaded :: Loaded
}

data Out = Out {
  animationCommands :: V.Vector Animation.Command,
  darkHexes :: [Hex],
  exit :: Bool,
  extraColor :: FighterId -> Maybe Color,
  ghostPlacing :: Maybe Placing,
  intent :: Maybe Annotation,
  lightHexes :: [Hex],
  loadRequests :: Set LoadRequest,
  soundCommands :: V.Vector Sound.Command
}

data Data = Data {
  plan :: Maybe Plan,
  frameNumber :: Int,
  subframeNumber :: Int
}

--------------------------------------------------------------------------------

with :: Deps -> ((In -> IO Out) -> IO a) -> IO a
with deps@(Deps {..}) next =
  C.with (C.Deps {..}) $ \core -> do
    let
      data_ = Data {
        plan = Nothing,
        frameNumber = 0,
        subframeNumber = 0
      }
    ref <- newIORef data_
    next $ \in_ -> do
      d0 <- readIORef ref
      (d1, out) <- run core d0 deps in_
      writeIORef ref d1
      return out

--------------------------------------------------------------------------------

slowdown :: Int -- XXX move to Config
slowdown = 1

run :: (C.In -> IO C.Out) -> Data -> Deps -> In -> IO (Data, Out)
run core (Data {..}) (Deps {..}) (In {..}) = do
  let
    cmds = do
      p <- plan
      p V.!? frameNumber
    --
    isActive0 = case cmds of
      Just _ -> False
      _ -> True
    --
    subframeNumber' = (subframeNumber + 1) `mod` slowdown
    frameNumber' =
      case cmds of
        Just _ ->
          if subframeNumber' == 0
          then frameNumber + 1
          else frameNumber
        Nothing -> 0
    --
    (animationCommands, soundCommands) =
      maybe (V.empty, V.empty) id $ do
        guard (subframeNumber == 0)
        cmds
  -- XXX should not call Core when animating!
  C.Out {..} <- core (C.In {..})
  --
  let
    (plan', loadRequests) =
      case cmds of
        Just _ -> (plan, empty)
        Nothing ->
          -- XXX will try to recompute plan each frame until resources are loaded
          case Plan.make loaded groupSizeOf update of
            Left l -> (Nothing, l)
            Right p -> (Just p, empty)
    --
  --
  return ((Data plan' frameNumber' subframeNumber'), (Out {..}))
