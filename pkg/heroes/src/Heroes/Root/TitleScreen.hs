module Heroes.Root.TitleScreen where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import qualified Heroes.Root.Common                        as Root
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data In = In {
  dispatch :: Root.Action -> IO ()
} deriving (Generic)

data Data = Data {}
  deriving (Generic)

run :: In -> Data -> IO Root.ScreenOut
run (In {..}) (Data {}) = do
  let
    drawCallback _ _ _ _ _ = return ()
    soundCommands = undefined
    intent = undefined
    loadRequests = undefined
  return (Root.ScreenOut {..})
