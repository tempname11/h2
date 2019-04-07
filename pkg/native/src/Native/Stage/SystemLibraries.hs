{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Stage.SystemLibraries (
  with,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Stage.SystemLibraries
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified System.Remote.Monitoring                  as EKG
import Data.String                                       (fromString)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

-- XXX remove?
instance SystemLibraries where
  with next = do
    void $ EKG.forkServer (fromString "localhost") 8010
    next $ Prov { noProv = () }
