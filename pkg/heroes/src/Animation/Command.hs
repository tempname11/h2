module Animation.Command where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Animation
import Animation.Scene
import Battle                                            (ObstacleId)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Command
  = HC Handle HCommand
  | PC ObstacleId PCommand
  | RemoveAll
  | SetCurtainOpacity Float
  deriving (Show)

data PCommand
  = PAdd Prop
  | PRemove
  deriving (Show)

data HCommand
  = SetPosition Position
  | SetHeight CInt
  | SetFacing Facing
  | SetGroupNumber GroupNumber
  | SetAnimated Bool
  | Add Actor
  | Remove
  deriving (Show)
