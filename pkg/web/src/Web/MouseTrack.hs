module Web.MouseTrack (
  new,
  disable,
  readLatest,
  Ref
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Input                                   (MouseButton (LMB, RMB))
import Web
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Bits                                      ((.&.))
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype Ref = Ref JSVal
instance IsJSVal Ref

foreign import javascript unsafe "h$newMouseTrackRef($1)"
  new :: Canvas -> IO Ref

foreign import javascript unsafe "h$disableMouseTrackRef($1)"
  disable :: Ref -> IO ()

foreign import javascript unsafe "$1.x"
  readX :: Ref -> IO Int

foreign import javascript unsafe "$1.y"
  readY :: Ref -> IO Int

foreign import javascript unsafe "$1.b"
  readB :: Ref -> IO Int

hasBit :: Int -> Int -> Bool
hasBit x b = (x .&. b) == b

readLatest :: Ref -> IO (Point V2 Int, MouseButton -> Bool)
readLatest ref = do
  x <- readX ref
  y <- readY ref
  b <- readB ref
  let place = P (V2 x y)
      mousePressed LMB = hasBit b 1
      mousePressed RMB = hasBit b 2
  return (place, mousePressed)
