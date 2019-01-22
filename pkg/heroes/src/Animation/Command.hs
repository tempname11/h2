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

data PCommand
  = PAdd Prop
  | PRemove

data HCommand
  = SetPosition Position
  | SetHeight CInt
  | SetFacing Facing
  | SetGroupNumber GroupNumber
  | SetAnimated Bool
  | Add Actor
  | Remove
