module Heroes.UI where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Heroes.H3                                         (SFX(..))
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

------------ utilities ---------------------------------------------------------

creatureGroundOffset :: V2 CInt
creatureGroundOffset = V2 192 256 -- approximate, but looks right

sfxGroundOffset :: SFX -> V2 CInt
sfxGroundOffset = \case -- approximate
  SFX'Haste -> V2 64 96
  SFX'Slow -> V2 32 32

viewportSize :: V2 Int
viewportSize = V2 800 600

fieldCenter :: Point V2 CInt
fieldCenter = P $ V2 388 314 -- approximate

legsOffset :: V2 CInt
legsOffset = V2 22 30 -- approximate

widthOffset :: V2 CInt
widthOffset = V2 44 0
