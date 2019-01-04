{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Config where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Platform.Config
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Config where
  productionPrefix = "../.production-assets/"
  staticSpriteExtension = ".png"
