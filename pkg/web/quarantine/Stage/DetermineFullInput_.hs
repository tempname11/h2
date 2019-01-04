module Web.Stage.DetermineFullInput_ (
  with,
  Deps (..),
  Out(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import qualified Heroes.Input                              as Input
import qualified Heroes.ControlMap                         as ControlMap

import qualified Stage.Links                               as L
import qualified Web.Stage.Links                           as L

import qualified Web.MouseTrack                            as MouseTrack
import qualified Web.KeyboardTrack                         as KeyboardTrack
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  theCanvas :: L.TheCanvas
}

data Out = Out {
  fullInput :: L.FullInput
}

--------------------------------------------------------------------------------

with :: Deps -> ((IO Out) -> IO a) -> IO a
with (Deps {..}) next = do
  mtr <- MouseTrack.new theCanvas
  ktr <- KeyboardTrack.new
  ref <- newIORef Input.zero

  result <- next $ do
    d0 <- readIORef ref
    (out, d1) <- external mtr ktr d0
    writeIORef ref d1
    return out

  MouseTrack.disable mtr
  KeyboardTrack.disable ktr
  return result

--------------------------------------------------------------------------------

type Data = Input.Current

--------------------------------------------------------------------------------

external :: MouseTrack.Ref -> KeyboardTrack.Ref -> Data -> IO (Out, Data)
external mtr ktr d0 = do
  (place, mousePressed) <- MouseTrack.readLatest mtr
  keyPressed <- KeyboardTrack.readLatest ktr
  let d1 = Input.Current (Just place) mousePressed keyPressed
                                -- XXX check if offscreen?
      fullInput = Input.toFull d0 d1
      quitEvent = False

  return (Out {..}, d1)
