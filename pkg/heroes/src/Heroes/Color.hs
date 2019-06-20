module Heroes.Color where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Color = V4 Word8

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
