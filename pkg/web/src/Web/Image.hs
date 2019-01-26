{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Image where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Image
import Web
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import JavaScript.Web.Canvas                             (Image)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

foreign import javascript unsafe "$1.naturalWidth"
  width' :: Image -> IO Int

foreign import javascript unsafe "$1.naturalHeight"
  height' :: Image -> IO Int

instance Capability'Image where
  type AnyImage = Image
  width = width'
  height = height'
