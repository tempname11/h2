module Heroes.Plan.Sound where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation.Scene                                   (Handle)
import Heroes
import Heroes.Plan.Common
import qualified Heroes.SND                                as SND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

playOnce :: Handle -> SND.Chunk -> Offset -> M0
playOnce h c = append (SND.PlayOnce h (Some c))

start :: Handle -> SND.Chunk -> Offset -> M0
start h c  = append (SND.Start h (Some c))

stop :: Handle -> Offset -> M0
stop h = append (SND.Stop h)

append :: SND.Command -> Offset -> M0
append c o = tell [(o, Right c)]
