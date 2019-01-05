{-# LANGUAGE RankNTypes #-}
module Stage.Blackbox (
  with,
  Deps (..),
  In (..),
  Out(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import qualified Animation.Command                         as Animation
import qualified Heroes.Plan                               as Plan
import qualified Stage.Core                                as C
import qualified Stage.Links                               as L
import Animation.Scene                                   (Handle(..))
import Heroes.Plan                                       (Plan)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  setup         :: L.Setup,
  initialBattle :: L.InitialBattle,
  groupSizeOf   :: L.GroupSizeOf
}

data In = In {
  isLoaded :: L.IsLoaded,
  fullInput :: L.FullInput
}

data Out = Out {
  animationCommands :: L.AnimationCommands,
  soundCommands     :: L.SoundCommands,
  ghostPlacing      :: L.GhostPlacing,
  extraColor        :: L.ExtraColor,
  lightHexes        :: L.LightHexes,
  darkHexes         :: L.DarkHexes,
  intent            :: L.Intent,
  exit              :: L.Exit
}

--------------------------------------------------------------------------------

with :: Deps -> ((In -> IO Out) -> IO a) -> IO a
with deps@(Deps {..}) next =
  C.with (C.Deps {..}) $ \core -> do
    let data_ = Data (Plan.fromBattle initialBattle) 0 0
    ref <- newIORef data_
    next $ \in_ -> do
      d0 <- readIORef ref
      (d1, out) <- run core d0 deps in_
      writeIORef ref d1
      return out

--------------------------------------------------------------------------------

data Data = Data {
  plan           :: Plan,
  frameNumber    :: Int,
  subframeNumber :: Int
}

--------------------------------------------------------------------------------

slowdown :: Int
slowdown = 1

canExecute :: L.IsLoaded -> [Animation.Command] -> Bool
canExecute isLoaded = all good
  where
  good c = case c of
    Animation.HC (Handle'Fighter f) (Animation.Add _) -> isLoaded (Right (f ^. creature_))
    Animation.HC (Handle'SFX sfx) (Animation.Add _) -> isLoaded (Left sfx)
    _ -> True

run :: (C.In -> IO C.Out) -> Data -> Deps -> In -> IO (Data, Out)
run core d (Deps {..}) (In {..}) = do
  let Data {plan, frameNumber, subframeNumber} = d
      cmds = plan V.!? frameNumber
      isActive0 = case cmds of
        Just _ -> False
        _ -> True
      subframeNumber' = (subframeNumber + 1) `mod` slowdown
      shouldIncrease = subframeNumber' == 0
  --
  C.Out {..} <- core (C.In {..})
  --
  -- XXX Kostyli ;)
  let plan' = case cmds of
                Just _  -> plan
                Nothing -> Plan.make groupSizeOf update
      --
      frameNumber' = case cmds of
                       Just (cs, _) ->
                         if shouldIncrease && canExecute isLoaded cs -- XXX a bit wrong
                         then frameNumber + 1
                         else frameNumber
                       Nothing -> 0
      --
      animationCommands = do
        f <- fmap fst cmds <|> return []
        guard shouldIncrease
        return f
      --
      soundCommands = fmap snd cmds
  --
  return ((Data plan' frameNumber' subframeNumber'), (Out {..}))
