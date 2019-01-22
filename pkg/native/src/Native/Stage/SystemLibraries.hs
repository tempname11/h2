{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Stage.SystemLibraries (
  with,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Stage.SystemLibraries
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified SDL.Mixer                                 as Mix
import qualified System.Remote.Monitoring                  as EKG
import Data.String                                       (fromString)
import Data.Default.Class                                (def)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance SystemLibraries where
  with next = do
    void $ EKG.forkServer (fromString "localhost") 8010
    Mix.initialize [
        Mix.InitMP3
      ]
    Mix.openAudio def 256
    --
    result <- next $ Prov { noProv = () }
    --
    Mix.closeAudio
    Mix.quit
    --
    return result
