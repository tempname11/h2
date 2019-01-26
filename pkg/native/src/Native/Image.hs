{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Image where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Image
import Native
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Vector.Storable                              (unsafeWith)
import Foreign.Ptr                                       (castPtr)
import Foreign.Storable                                  (Storable)
import qualified Codec.Picture                             as Juicy
import qualified Codec.Picture.Types                       as Juicy'
import qualified Data.Vector.Storable                      as SV
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Capability'Image where
  type AnyImage = Juicy.DynamicImage
  width i = return (Juicy'.dynamicMap Juicy.imageWidth i)
  height i = return (Juicy'.dynamicMap Juicy.imageHeight i)

withImagePtr :: AnyImage -> (Ptr () -> IO a) -> IO a
withImagePtr image = Juicy'.dynamicMap (withPtr . Juicy.imageData) image
  where
  withPtr :: Storable a => SV.Vector a -> (Ptr () -> IO b) -> IO b
  withPtr v f = unsafeWith v (f . castPtr)
