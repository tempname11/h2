{-# LANGUAGE TemplateHaskell #-}
module Animation.Scene (
  Handle(..),
  Actor(..),
  Prop(..),
  Scene(..),
  Position,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle                                            (FighterId)
import Battle                                            (ObstacleId)
import Heroes
import qualified Heroes.Platform                           as Platform
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Position = Point V2 CInt

data Handle
  = Handle'Fighter FighterId
  | Handle'SFX SFX
  deriving (Eq, Ord, Show)

data Actor = Actor {
  sprite :: Platform.ComplexSprite,
  position :: Position,
  height :: CInt,
  facing :: Facing,
  groupN :: Int, -- XXX GroupNumber
  frameN :: Int,
  subframeN :: Int,
  animated :: Bool
}

data Prop = Prop {
  position :: Position,
  facing :: Facing
}

data Scene = Scene {
  actors :: Map Handle Actor,
  props :: Map ObstacleId Prop,
  curtain :: Float
}

makeShorthands ''Actor
makeShorthands ''Prop
makeShorthands ''Scene
