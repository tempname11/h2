module Stage.DetermineInput where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import qualified Heroes.Input                              as Input
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data Out = Out {
  fullInput :: Input.Full
}

class DetermineInput where
  with :: Deps -> ((IO Out) -> IO a) -> IO a
