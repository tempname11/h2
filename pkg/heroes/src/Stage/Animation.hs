module Stage.Animation (
  with,
  Deps (..),
  In (..),
  Out(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Animation
import Animation.Command
import Animation.Scene
import qualified Stage.Links                               as L
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  groupSizeOf :: L.GroupSizeOf
}

data In = In {
  animationCommands :: L.AnimationCommands
}

data Out = Out {
  scene :: Scene
}

--------------------------------------------------------------------------------

with :: Deps -> ((In -> IO Out) -> IO a) -> IO a
with (Deps {..}) next = do
  ref <- newIORef $
    Scene {
      actors = empty,
      props = empty,
      curtain = 1.0
    }
  next $ \in_ -> do
    d <- readIORef ref
    let (d', out) = run groupSizeOf d in_
    writeIORef ref d'
    return out

--------------------------------------------------------------------------------

run :: GroupSizeOf -> Scene -> In -> (Scene, Out)
run gso scene (In {..}) = (scene', out)
  where
  scene' = t scene
  out = Out {scene = scene'}
  --
  t :: Scene -> Scene
  t = case animationCommands of
    Just cs -> t' cs
    Nothing -> id
  --
  t' :: [Command] -> Scene -> Scene
  t' cs = increment gso >>> applyAll cs

increment :: GroupSizeOf -> Scene -> Scene
increment groupSizeOf = over actors_ $ M.mapWithKey $ \h -> execState $ do
  a <- use animated_
  when a $ do
    g <- use groupN_
    let GroupSize n = groupSizeOf h (GroupNumber g)
    s <- subframeN_ <%= ((+1) >>> (`mod` 4))
    when (s == 0) $
      frameN_ %= ((+1) >>> (`mod` n))

applyAll :: [Command] -> Scene -> Scene
applyAll cs s = foldr' apply s cs

apply :: Command -> Scene -> Scene
apply RemoveAll = set actors_ empty >>> set props_ empty
apply (SetCurtainOpacity x) = set curtain_ x
apply (PC o c) = case c of
  PAdd a ->
    set (props_ . at o) (Just a)
  PRemove ->
    set (props_ . at o) Nothing
apply (HC h c) = case c of
  SetPosition p ->
    setMayX (actors_ . by h . position_) p
  SetHeight z ->
    setMayX (actors_ . by h . height_) z
  SetFacing f ->
    setMayX (actors_ . by h . facing_) f
  SetGroupNumber (GroupNumber g) ->
    setMayX (actors_ . by h . groupN_) g >>>
    setMayX (actors_ . by h . frameN_) 0 >>>
    setMayX (actors_ . by h . subframeN_) 0
  SetAnimated b ->
    setMayX (actors_ . by h . animated_ ) b
  Add a ->
    set (actors_ . at h) (Just a)
  Remove ->
    set (actors_ . at h) Nothing


setMayX :: ((a -> Maybe b) -> s -> Maybe s) -> b -> s -> s
setMayX l x s = maybe s id (setMay l x s)
