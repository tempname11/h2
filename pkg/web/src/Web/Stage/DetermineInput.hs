{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Stage.DetermineInput () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import Web.Platform ()
import Stage.DetermineInput
import qualified Heroes.Input                              as Input
import qualified Web.KeyboardTrack                         as KeyboardTrack
import qualified Web.MouseTrack                            as MouseTrack
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance DetermineInput where
  with (Deps {..}) next = do
    mtr <- MouseTrack.new inputProvider
    ktr <- KeyboardTrack.new
    ref <- newIORef Input.zero
    --
    result <- next $ do
      d0 <- readIORef ref
      (out, d1) <- external mtr ktr d0
      writeIORef ref d1
      return out
    --
    MouseTrack.disable mtr
    KeyboardTrack.disable ktr
    return result

type Data = Input.Current

external :: MouseTrack.Ref -> KeyboardTrack.Ref -> Data -> IO (Out, Data)
external mtr ktr d0 = do
  (place, mousePressed) <- MouseTrack.readLatest mtr
  keyPressed <- KeyboardTrack.readLatest ktr
  let d1 = Input.Current (Just place) mousePressed keyPressed
                                -- XXX check if offscreen?
      fullInput = Input.toFull d0 d1 quitEvent
      quitEvent = False
  --
  return (Out {..}, d1)
