module Stage.Links where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle
import Heroes
import Heroes.Aux
import Heroes.UI
import qualified Animation
import qualified Animation.Command
import qualified Battle.AM
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type AnimationCommands = Maybe [Animation.Command.Command]
type DarkHexes = [Hex]
type Exit = Bool
type ExtraColor = FighterId -> Maybe Color
type GhostPlacing = Maybe Placing
type GroupSizeOf = Animation.GroupSizeOf
type Intent = Maybe Annotation
type IsActive = Bool
type LightHexes = [Hex]
type Update = Battle.AM.Update
type Wishes = [Either SFX Creature]
