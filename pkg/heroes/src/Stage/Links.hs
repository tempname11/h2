{-# LANGUAGE RankNTypes #-}
module Stage.Links where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle
import Heroes
import Heroes.Aux
import Heroes.UI
import qualified Animation
import qualified Animation.Command
import qualified Animation.Scene
import qualified Battle.AM
import qualified Battle.Setup
import qualified Heroes.Essentials
import qualified Heroes.Input
import qualified Heroes.UI.Sound
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type AnimationCommands = Maybe [Animation.Command.Command]
type DarkHexes = [Hex]
type Essentials = Heroes.Essentials.Essentials
type Exit = Bool
type ExtraColor = FighterId -> Maybe Color
type FullInput = Heroes.Input.Full
type GhostPlacing = Maybe Placing
type GroupSizeOf = Animation.GroupSizeOf
type InitialBattle = Battle
type Intent = Maybe Annotation
type IsActive = Bool
type LightHexes = [Hex]
type Scene = Animation.Scene.Scene
type Setup = Battle.Setup.Setup
type SoundCommands = Maybe [Heroes.UI.Sound.Command]
type Update = Battle.AM.Update
type Wishes = [Either SFX Creature]
