module Heroes.Plan.Sound where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Plan.Types
import qualified Heroes.UI.Sound                           as Sound
import Heroes.UI.Sound                                   (Sound)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

playOnce :: Sound -> Offset -> M0
playOnce w = append (Sound.PlayOnce w)

start :: Sound -> Offset -> M0
start w = append (Sound.Start w)

stop :: Sound -> Offset -> M0
stop w = append (Sound.Stop w)

append :: Sound.Command -> Offset -> M0
append c o = tell [(o, Right c)]
