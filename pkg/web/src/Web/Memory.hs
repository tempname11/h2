{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Memory where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Memory
import Web
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Unsafe.Coerce                                     (unsafeCoerce)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

foreign import javascript unsafe
  "$r = new ArrayBuffer($1);"
  create' :: Int -> IO Buffer

foreign import javascript unsafe
  "$r = new Float32Array($1);"
  cast' :: Buffer -> IO F32

foreign import javascript unsafe
  "$1[$2] = $3;"
  write' :: F32 -> Int -> Float -> IO ()

instance Memory where
  type Buffer = JSVal
  type F32 = JSVal
  create = create'
  cast = cast'
  write = write'
  delete _ = return ()
  coax = hack' . unsafeCoerce

foreign import javascript unsafe
  "$1.d2.d1.buf" -- REALLY unsafe
  hack' :: Float -> JSVal
