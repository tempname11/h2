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
  with _ next = next $ const $ return () -- XXX shim
