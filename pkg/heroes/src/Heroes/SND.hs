module Heroes.SND where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Animation.Scene                                   (Handle)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data Prov = Prov {
  playSounds :: Handler In
}

data In = In {
  soundCommands :: V.Vector Command
}

class SND where
  type Chunk
  loadChunk :: String -> IO Chunk
  freeChunk :: Chunk -> IO ()
  --
  with :: Deps -> With Prov

data Command
  = PlayOnce Handle (Some Chunk)
  | Start Handle (Some Chunk)
  | Stop Handle
  --
  deriving (Generic, Show)
