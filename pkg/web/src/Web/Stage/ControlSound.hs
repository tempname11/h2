{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Stage.ControlSound (
  with,
  Deps (..),
  In (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Stage.Blackbox                                    (SoundCommands)
import Stage.ControlSound
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data In = In {
  soundCommands :: SoundCommands
}

instance ControlSound where
  with _ next = next $ const $ return () -- XXX shim
