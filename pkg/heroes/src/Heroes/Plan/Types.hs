module Heroes.Plan.Types where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Animation
import qualified Animation.Command                         as Animation
import qualified Heroes.UI.Sound                           as Sound
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype Offset = Offset Int

type Cmds = ([Animation.Command], [Sound.Command])
type S a = State (IntMap Cmds) a
type S0 = S ()

-- XXX very dubious naming and idea (custom monad instead?)
type Sh = GroupSizeOf -> Offset -> S Offset -- SHortcut

type Plan = V.Vector Cmds
