{-# LANGUAGE TemplateHaskell #-}
module Animation.Scene (
  Handle(..),
  Actor(..),
  Prop(..),
  Scene(..),
  Position,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Battle                                            (FighterId)
import Battle                                            (ObstacleId)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Position = Point V2 CInt

data Handle
  = Handle'Fighter FighterId
  | Handle'SFX SFX
  deriving (Eq, Ord, Show)

data Actor = Actor {
  position  :: Position,
  height    :: CInt,
  facing    :: Facing,
  groupN    :: Int, -- XXX GroupNumber
  frameN    :: Int,
  subframeN :: Int,
  animated  :: Bool
} deriving (Show)

data Prop = Prop {
  position :: Position,
  facing   :: Facing
} deriving (Show)

data Scene = Scene {
  actors :: Map Handle Actor,
  props :: Map ObstacleId Prop,
  curtain :: Float
} deriving (Show)

--------------------------------------------------------------------------------

makeShorthands ''Actor
makeShorthands ''Scene
