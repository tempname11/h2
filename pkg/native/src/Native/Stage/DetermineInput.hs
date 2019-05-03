{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Stage.DetermineInput () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Stage.DetermineInput
import qualified Heroes.Input                              as Input
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance DetermineInput where
  with _ next = do
    ref <- newIORef initially
    next $ do
      d0 <- readIORef ref
      (out, d1) <- external d0
      writeIORef ref d1
      return out

--------------------------------------------------------------------------------

type Data = Input.Snapshot

--------------------------------------------------------------------------------

initially :: Data
initially = Input.Snapshot offScreen none none
  where
  none = const False
  offScreen = Nothing

external :: Data -> IO (Out, Data)
external rw0 = do
  events <- collectJustsM SDL.pollEvent
  rw1 <- Input.Snapshot
    <$> (Just . (<§>) <$> SDL.getAbsoluteMouseLocation) -- XXX check if offscreen.
    <*> ((. mousecode) <$> SDL.getMouseButtons)
    <*> ((. scancode) <$> SDL.getKeyboardState)

  let fullInput = Input.toFull rw0 rw1 quitEvent
      quitEvent = any isQuitEvent events

  return (Out {..}, rw1)

--------------------------------------------------------------------------------

scancode :: Input.Key -> SDL.Scancode
scancode = \case
  Input.Key'Escape -> SDL.ScancodeEscape
  Input.Key'Enter -> SDL.ScancodeReturn
  Input.Key'Left -> SDL.ScancodeLeft
  Input.Key'Right -> SDL.ScancodeRight
  Input.Key'R -> SDL.ScancodeR
  Input.Key'1 -> SDL.Scancode1
  Input.Key'2 -> SDL.Scancode2
  Input.Key'3 -> SDL.Scancode3

mousecode :: Input.MouseButton -> SDL.MouseButton
mousecode = \case
  Input.LMB -> SDL.ButtonLeft
  Input.RMB -> SDL.ButtonRight

isQuitEvent :: SDL.Event -> Bool
isQuitEvent = \case
  SDL.Event _ SDL.QuitEvent -> True
  _                         -> False

