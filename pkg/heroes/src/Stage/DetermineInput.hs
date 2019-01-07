module Stage.DetermineInput where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import qualified Heroes.Input                              as Input
import qualified Heroes.Platform                           as Platform
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  inputProvider :: Platform.InputProvider
}

data Out = Out {
  fullInput :: Input.Full
}

class DetermineInput where
  with :: Deps -> ((IO Out) -> IO a) -> IO a
