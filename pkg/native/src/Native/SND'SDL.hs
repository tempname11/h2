{-# OPTIONS_GHC -Wno-orphans #-}
module Native.SND'SDL () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation.Scene                                   (Handle)
import Heroes.SND
import Native
import Native.Platform ()
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified SDL.Mixer                                 as Mix
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Data = Map Handle Mix.Channel

instance SND where
  type Chunk = Mix.Chunk
  loadChunk = Mix.load
  freeChunk = Mix.free
  --
  with _ next = do
    ref <- newIORef empty
    Mix.initialize [Mix.InitMP3]
    Mix.openAudio def 256
    let
      playSounds = \in_ -> do
        d0 <- readIORef ref
        d1 <- sound in_ d0
        writeIORef ref d1
    next $ Prov {..}
    Mix.closeAudio
    Mix.quit

--------------------------------------------------------------------------------

sound :: In -> Data -> IO Data
sound (In {..}) =
  execStateT $
    for_ soundCommands $ \case
      PlayOnce h (Some c) -> do
        channel <- lift $ Mix.playOn (-1) Mix.Once c
        assign (at h) (Just channel)
      Start h (Some c) -> do
        channel <- lift $ Mix.playOn (-1) Mix.Forever c
        assign (at h) (Just channel)
      Stop h _ -> do
        m <- use $ at h
        case m of
          Nothing -> do
            lift $ print ("No sound playing for:" :: String, h)
            return ()
          Just channel -> do
            lift $ Mix.halt channel
