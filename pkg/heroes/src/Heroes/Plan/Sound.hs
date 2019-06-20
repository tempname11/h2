module Heroes.Plan.Sound where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Plan.Common
import qualified Heroes.SND                                as SND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

playOnce :: Handle -> SND.Chunk -> Offset -> M0
playOnce h c = append (SND.PlayOnce h (Some c))

start :: Handle -> SND.Chunk -> Offset -> M0
start h c  = append (SND.Start h (Some c))

stop :: Handle -> SND.Chunk -> Offset -> M0
stop h c = append (SND.Stop h (Some c))

append :: SND.Command -> Offset -> M0
append c o = tell [(o, Right c)]
