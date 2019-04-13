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

-- colors

type Color = V4 Word8

semitransparent :: Color -> Color
semitransparent (V4 r g b a) = V4 r g b (div a 2)

black :: Color
black = V4 0 0 0 255

white :: Color
white = V4 255 255 255 255

red :: Color
red = V4 255 0 0 255

green :: Color
green = V4 0 255 0 255

blue :: Color
blue = V4 0 0 255 255

yellow :: Color
yellow = V4 255 255 0 255

cyan :: Color
cyan = V4 0 255 255 255

magenta :: Color
magenta = V4 255 0 255 255

transparent :: Color
transparent = V4 0 0 0 0
