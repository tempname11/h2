module Heroes.Root.Common where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle                                            (Battle)
import Battle.Setup                                      (Setup)
import Heroes
import Stage.LoadingThread                               (LoadRequest(..))
import qualified Heroes.GFX                                as GFX
import qualified Heroes.SND                                as SND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Out = Out {
  exit :: Bool,
  drawCallback :: GFX.DrawCallback,
  soundCommands :: V.Vector SND.Command,
  intent :: Maybe Annotation,
  loadRequests :: Set LoadRequest
} deriving (Generic)

data Action
  = Action'ExitScreen
  | Action'StartBattle {
    setup :: Setup,
    initialBattle :: Battle
  }
  deriving (Generic, Show)
