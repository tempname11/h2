module Heroes.Root.Common where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Aux                                        (Annotation)
import Stage.LoadingThread                               (LoadRequest(..))
import qualified Heroes.GFX                                as GFX
import qualified Heroes.SND                                as SND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data ScreenOut = ScreenOut {
  drawCallback :: GFX.DrawCallback,
  soundCommands :: V.Vector SND.Command,
  intent :: Maybe Annotation,
  loadRequests :: Set LoadRequest
} deriving (Generic)
