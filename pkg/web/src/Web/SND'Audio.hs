{-# OPTIONS_GHC -Wno-orphans #-}
module Web.SND'Audio where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Heroes.SND                                        
import qualified Web.Audio                                 as Audio
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance SND where
  type Chunk = Audio.Audio
  loadChunk = Audio.load
  freeChunk _ = return () -- XXX
  with _ next = do
    let
      -- XXX: this simple implementation relies on not playing the same chunk
      -- multiple times at once
      playSounds = \(In {..}) -> for_ soundCommands $ \case
        PlayOnce _ (Some c) -> Audio.playOnce c
        Start _ (Some c) -> Audio.start c
        Stop _ (Some c) -> Audio.stop c
    next (Prov {..})
