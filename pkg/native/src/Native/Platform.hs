{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Platform
import Native.Image ()
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Codec.Picture                             as Juicy
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (forkIO)

instance Platform where
  productionPrefix = ".production-assets/"
  forkPreferred = forkIO
  loadImage = Juicy.readPng
