module Native.Stage.DetermineFullInput_ (
  with,
  Deps (..),
  Out(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native

import qualified Stage.Links                               as L

import qualified Heroes.Input                              as Input
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data Out = Out {
  fullInput :: L.FullInput
}

--------------------------------------------------------------------------------

with :: Deps -> ((IO Out) -> IO a) -> IO a
with _ next = do
  ref <- newIORef initially
  next $ do
    d0 <- readIORef ref
    (out, d1) <- external d0
    writeIORef ref d1
    return out

--------------------------------------------------------------------------------

type Data = Input.Current

--------------------------------------------------------------------------------

initially :: Data
initially = Input.Current offScreen none none
  where
  none = const False
  offScreen = Nothing

external :: Data -> IO (Out, Data)
external rw0 = do
  events <- collectJustsM SDL.pollEvent
  rw1 <- Input.Current
    <$> (Just . (<§>) <$> SDL.getAbsoluteMouseLocation) -- XXX check if offscreen.
    <*> ((. mousecode) <$> SDL.getMouseButtons)
    <*> ((. scancode) <$> SDL.getKeyboardState)

  let fullInput = Input.toFull rw0 rw1 quitEvent
      quitEvent = any isQuitEvent events

  return (Out {..}, rw1)

--------------------------------------------------------------------------------

scancode :: Input.Key -> SDL.Scancode
scancode = \case
  Input.Escape -> SDL.ScancodeEscape
  Input.Left   -> SDL.ScancodeLeft
  Input.Right  -> SDL.ScancodeRight
  Input.R      -> SDL.ScancodeR
  Input.N1     -> SDL.Scancode1
  Input.N2     -> SDL.Scancode2
  Input.N3     -> SDL.Scancode3

mousecode :: Input.MouseButton -> SDL.MouseButton
mousecode = \case
  Input.LMB -> SDL.ButtonLeft
  Input.RMB -> SDL.ButtonRight

isQuitEvent :: SDL.Event -> Bool
isQuitEvent = \case
  SDL.Event _ SDL.QuitEvent -> True
  _                         -> False

