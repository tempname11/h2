{-# LANGUAGE RankNTypes #-}
module Heroes.Root.BattleScreen.Blackbox (
  run,
  Data(..),
  In(..),
  Out(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation                                         (GroupSizeOf)
import Battle                                            (Battle)
import Battle                                            (FighterId)
import Battle.Setup                                      (Setup)
import Heroes
import Heroes.AAI                                        (AIQuery(..))
import Heroes.AAI                                        (AIResult(..))
import Heroes.Aux                                        (Annotation)
import Heroes.Plan                                       (Plan)
import Heroes.Color                                      (Color)
import Stage.Loading                                     (Loaded)
import Stage.LoadingThread                               (LoadRequest(..))
import qualified Animation.Command                         as Animation
import qualified Battle.AM                                 as AM
import qualified Heroes.Input                              as Input
import qualified Heroes.Plan                               as Plan
import qualified Heroes.Root.BattleScreen.Core             as C
import qualified Heroes.SND                                as SND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data In = In {
  initialBattle :: Battle,
  setup :: Setup,
  queryAI :: IO (Maybe AIResult),
  askAI :: Maybe AIQuery -> IO (),
  groupSizeOf :: GroupSizeOf,
  fullInput :: Input.Full,
  loaded :: Loaded
}

data Out = Out {
  animationCommands :: V.Vector Animation.Command,
  darkHexes :: V.Vector Hex,
  exit :: Bool,
  extraColor :: FighterId -> Maybe Color,
  intent :: Maybe Annotation,
  lightHexes :: V.Vector Hex,
  loadRequests :: Set LoadRequest,
  soundCommands :: V.Vector SND.Command
}

data Data = Data {
  updateOrPlan :: Either AM.Update Plan,
  frameNumber :: Int,
  subframeNumber :: Int
}

slowdown :: Int -- XXX move to Config
slowdown = 1

run :: (C.In -> IO C.Out) -> IORef Data -> In -> IO Out
run core ref (In {..}) = do
  Data {..} <- readIORef ref
  let
    cmds = do
      p <-
        case updateOrPlan of
          Left _ -> Nothing
          Right p -> Just p
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
    (updateOrPlan', loadRequests) =
      case cmds of
        Just _ -> (updateOrPlan, empty)
        Nothing ->
          let
            realUpdate =
              case updateOrPlan of
                Left u -> u
                Right _ -> update -- XXX hacky as hell
          in
            -- XXX will try to recompute plan each frame,
            -- until resources are loaded
            case Plan.make loaded groupSizeOf realUpdate of
              Left l -> (Left realUpdate, l)
              Right p -> (Right p, empty)
  --
  writeIORef ref $
    Data {
      updateOrPlan = updateOrPlan',
      frameNumber = frameNumber',
      subframeNumber = subframeNumber'
    }
  --
  return (Out {..})
