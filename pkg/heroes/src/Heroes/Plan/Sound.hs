module Heroes.Plan.Sound where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Plan.Types
import qualified Heroes.UI.Sound                           as Sound
import Heroes.UI.Sound                                   (Sound)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

playOnce :: Sound -> Offset -> S0
playOnce w o = append (Sound.PlayOnce w) o

start :: Sound-> Offset -> S0
start w o = append (Sound.Start w) o

stop :: Sound-> Offset -> S0
stop w o = append (Sound.Stop w) o

append :: Sound.Command -> Offset -> S0
append sc (Offset i) = at i %= f
  where
  f Nothing = Just ([], [])
  f (Just (acs, scs)) = Just (acs, sc : scs)
