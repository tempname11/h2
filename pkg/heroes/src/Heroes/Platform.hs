{-# LANGUAGE FlexibleContexts #-}
module Heroes.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Heroes.Image                                      (AnyImage)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class Platform where
  productionPrefix :: String
  -- XXX ImageHelpers?
  loadImage :: String -> IO (Either String AnyImage)
