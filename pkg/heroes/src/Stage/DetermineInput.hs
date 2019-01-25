module Stage.DetermineInput where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import qualified Heroes.Input                              as Input
import qualified Heroes.WND                                as WND
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  window :: WND.Window
}

data Out = Out {
  fullInput :: Input.Full
}

class DetermineInput where
  with :: Deps -> ((IO Out) -> IO a) -> IO a
