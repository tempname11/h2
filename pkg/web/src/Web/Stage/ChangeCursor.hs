{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Stage.ChangeCursor () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Stage.ChangeCursor
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance ChangeCursor where
  with _ next = next $ const $ return () -- XXX shim