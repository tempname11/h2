{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Stage.ControlSound where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Stage.ControlSound
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance ControlSound where
  with _ next = next $ const $ return () -- XXX shim
