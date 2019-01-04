{-# LANGUAGE TemplateHaskell #-}
module Native.SystemLibraries (
  SDLToken,
  MixToken,
  with,
  Prov (..)
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified SDL
import qualified SDL.Mixer                                 as Mix
import qualified System.Remote.Monitoring                  as EKG
import Data.String                                       (fromString)
import Data.Default.Class                                (def)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data SDLToken = SDLToken;
data MixToken = MixToken;

data Prov = Prov {
  sdlToken :: SDLToken,
  mixToken :: MixToken
}

with :: (Prov -> IO a) -> IO a
with next = do
  void $ EKG.forkServer (fromString "localhost") 8000
  SDL.initialize [
      SDL.InitAudio,
      SDL.InitVideo, 
      SDL.InitEvents
    ]
  Mix.initialize [
      Mix.InitMP3
    ]
  Mix.openAudio def 256
  --
  result <- next $ Prov SDLToken MixToken
  --
  Mix.closeAudio
  Mix.quit
  SDL.quit
  --
  return result
