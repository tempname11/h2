module Heroes.Memory where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class Memory where
  type Buffer
  type F32
  create :: Int -> IO Buffer
  cast :: Buffer -> IO F32
  write :: F32 -> Int -> Float -> IO ()
  delete :: Buffer -> IO ()
  -- XXX
  coax :: a -> Buffer 
