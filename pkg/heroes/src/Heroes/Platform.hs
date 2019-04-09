{-# LANGUAGE FlexibleContexts #-}
module Heroes.Platform where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Heroes.Image                                      (AnyImage)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Concurrent                                (ThreadId)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

class Platform where
  productionPrefix :: String
  forkPreferred :: IO () -> IO ThreadId
  -- XXX ImageHelpers?
  loadImage :: String -> IO (Either String AnyImage)
