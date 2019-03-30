module Stage.Animation (
  with,
  Deps (..),
  In (..),
  Out(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation
import Animation.Command
import Animation.Scene
import Heroes
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  groupSizeOf :: GroupSizeOf
}

data In = In {
  animationCommands :: V.Vector Command
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
run gso scene (In {..}) = (scene', Out { scene = scene' })
  where
  scene' = increment gso >>> applyAll animationCommands $ scene

increment :: GroupSizeOf -> Scene -> Scene
increment groupSizeOf = over _actors $ M.mapWithKey $ \h -> execState $ do
  a <- use _animated
  when a $ do
    g <- use _groupN
    let GroupSize n = groupSizeOf h (GroupNumber g)
    let _15fps s = s `mod` 4 == 0
    s <- _subframeN <%= (+1)
    when (_15fps s) $
      _frameN %= ((+1) >>> (`mod` n))

applyAll :: V.Vector Command -> Scene -> Scene
applyAll cs s = foldr' apply s cs

apply :: Command -> Scene -> Scene
apply RemoveAll = set _actors empty >>> set _props empty
apply (SetCurtainOpacity x) = set _curtain x
apply (PC o c) = case c of
  PAdd a ->
    set (_props . at o) (Just a)
  PRemove ->
    set (_props . at o) Nothing
apply (HC h c) = case c of
  SetPosition p ->
    setMayX (_actors . by h . _position) p
  SetHeight z ->
    setMayX (_actors . by h . _height) z
  SetFacing f ->
    setMayX (_actors . by h . _facing) f
  SetGroupNumber (GroupNumber g) ->
    setMayX (_actors . by h . _groupN) g >>>
    setMayX (_actors . by h . _frameN) 0 >>>
    setMayX (_actors . by h . _subframeN) 0
  SetAnimated b ->
    setMayX (_actors . by h . _animated) b
  Add a ->
    set (_actors . at h) (Just a)
  Remove ->
    set (_actors . at h) Nothing


setMayX :: ((a -> Maybe b) -> s -> Maybe s) -> b -> s -> s
setMayX l x s = maybe s id (setMay l x s)
