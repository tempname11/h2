module Heroes.Plan.Types where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation
import Heroes
import Stage.Loading                                     (Loaded)
import Stage.LoadingThread                               (LoadRequest)
import qualified Animation.Command                         as Animation
import qualified Heroes.UI.Sound                           as Sound
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Set                                  as S
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype Offset = Offset Int
  deriving (Show)
type M0 = M ()
type Plan = V.Vector (V.Vector Animation.Command, V.Vector Sound.Command)

type M =
  ReaderT (GroupSizeOf, Loaded) (
    StateT Offset (
      WriterT [(Offset, Either Animation.Command Sound.Command)] (
        Either (Set LoadRequest)
      )
    )
  )

loadRequest :: LoadRequest -> M a
loadRequest r = lift . lift . lift $ Left (S.singleton r)
