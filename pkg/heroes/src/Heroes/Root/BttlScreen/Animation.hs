module Heroes.Root.BttlScreen.Animation (
  run,
  Data,
  In (..)
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation
import Animation.Command
import Animation.Scene
import Heroes
import Heroes.Essentials                                 (Essentials)
import Heroes.Essentials                                 (groupSizeOf)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Data = Scene

data In = In {
  essentials :: Essentials,
  animationCommands :: V.Vector Command
}

run :: In -> Scene -> Scene
run (In {..}) = increment essentials >>> applyAll animationCommands

increment :: Essentials -> Scene -> Scene
increment essentials = over #actors $ M.mapWithKey $ \h -> execState $ do
  a <- use #animated
  when a $ do
    g <- use #groupN
    let GroupSize n = groupSizeOf essentials h (GroupNumber g)
    let _15fps s = s `mod` 4 == 0
    s <- #subframeN <%= (+1)
    when (_15fps s) $
      #frameN %= ((+1) >>> (`mod` n))

applyAll :: V.Vector Command -> Scene -> Scene
applyAll cs s = foldr' apply s cs

apply :: Command -> Scene -> Scene
apply RemoveAll = set #actors empty >>> set #props empty
apply (SetCurtainOpacity x) = set #curtain x
apply (PC o c) = case c of
  PAdd a ->
    set (#props . at o) (Just a)
  PRemove ->
    set (#props . at o) Nothing
apply (HC h c) = case c of
  SetPosition p ->
    setMayX (#actors . by h . #position) p
  SetHeight z ->
    setMayX (#actors . by h . #height) z
  SetFacing f ->
    setMayX (#actors . by h . #facing) f
  SetGroupNumber (GroupNumber g) ->
    setMayX (#actors . by h . #groupN) g >>>
    setMayX (#actors . by h . #frameN) 0 >>>
    setMayX (#actors . by h . #subframeN) 0
  SetAnimated b ->
    setMayX (#actors . by h . #animated) b
  Add a ->
    set (#actors . at h) (Just a)
  Remove ->
    set (#actors . at h) Nothing


setMayX :: ((a -> Maybe b) -> s -> Maybe s) -> b -> s -> s
setMayX l x s = maybe s id (setMay l x s)
