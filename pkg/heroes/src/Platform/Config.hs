module Platform.Config where

import Common

class Config where
  productionPrefix :: String
  staticSpriteExtension :: String

