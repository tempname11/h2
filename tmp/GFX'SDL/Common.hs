{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Native.GFX'SDL.Common where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.GFX
import Native
import qualified Heroes.Atlas                              as Atlas
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance GFX'Types where
  type StaticSprite = StaticSprite'SDL
  type ComplexSprite = ComplexSprite'SDL

data StaticSprite'SDL = StaticSprite'SDL {
  texture    :: SDL.Texture,
  dimensions :: V2 CInt
}

data ComplexSprite'SDL = ComplexSprite'SDL {
  surface :: SDL.Surface,
  palette :: SDL.Palette,
  groups  :: V.Vector Atlas.Group
}

makeShorthands ''StaticSprite'SDL
makeShorthands ''ComplexSprite'SDL
