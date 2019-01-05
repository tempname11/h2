{-# LANGUAGE TemplateHaskell #-}
module Heroes.StaticResources where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.H3.Misc                                    (ObstacleType)
import Heroes.Platform                                   (StaticSprite)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data StaticResources = StaticResources {
  cellShaded :: StaticSprite,
  cellOutline :: StaticSprite,
  background :: StaticSprite,
  obstacles :: ObstacleType -> StaticSprite
}

makeShorthands ''StaticResources
