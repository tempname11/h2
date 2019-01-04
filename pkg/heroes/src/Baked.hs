{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Baked where

import Data.Proxy
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Data.Proxy                                        (Proxy)
import Data.Bits                                         (shiftL)
import Data.Bits                                         (complement)
import Data.Bits                                         ((.|.))
import Data.Bits                                         ((.&.))
import Prelude                                           (fromEnum)
import Prelude                                           (toEnum)
import Control.Lens                                      (Lens')
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class Baked b where
  type Lower b :: *
  type Upper b :: *
  lowerBits :: Proxy b -> Int
  wrap :: Int -> b
  unwrap :: b -> Int

baked ::
  forall b.
  (Baked b, Enum (Lower b), Enum (Upper b)) =>
  Lower b -> Upper b -> b
baked l u = wrap (shiftL (fromEnum u) (lowerBits (Proxy @b)) + fromEnum l)

lower_ ::
  forall b.
  (Baked b, Enum (Lower b)) =>
  Lens' b (Lower b)
lower_ f i = fmap (setL i) (f (getL i))
  where
  lower8 = 2 ^ lowerBits (Proxy @b) - 1
  getL i' = toEnum (unwrap i' .&. lower8)
  setL i' c = wrap ((unwrap i' .|. complement lower8) + fromEnum c)
