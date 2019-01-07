{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Stage.SystemLibraries (
  with,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Stage.SystemLibraries
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance SystemLibraries where
  with next = next $ Prov { noProv = () }
