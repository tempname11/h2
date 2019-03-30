{-# LANGUAGE FlexibleContexts #-}
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
import Heroes.GFX'Types                                  (ComplexSprite)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import GHC.Generics (Generic)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Position = Point V2 CInt

data Handle
  = Handle'Fighter FighterId
  | Handle'SFX SFX
  deriving (Eq, Ord, Show)

data Actor = Actor {
  sprite :: Some ComplexSprite,
  position :: Position,
  height :: CInt,
  facing :: Facing,
  groupN :: Int, -- XXX GroupNumber
  frameN :: Int,
  subframeN :: Int,
  animated :: Bool
} deriving (Show, Generic)

data Prop = Prop {
  position :: Position,
  facing :: Facing
} deriving (Show, Generic)

data Scene = Scene {
  actors :: Map Handle Actor,
  props :: Map ObstacleId Prop,
  curtain :: Float
} deriving (Show, Generic)
