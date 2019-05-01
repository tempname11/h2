module Heroes.Root.TitleScreen where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Root.Common                                (ScreenOut(..))
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Data = Data {}
  deriving (Generic)

run :: Data -> IO ScreenOut
run (Data {}) = do
  let
    drawCallback _ _ _ _ _ = return ()
    soundCommands = undefined
    intent = undefined
    loadRequests = undefined
  return (ScreenOut {..})
