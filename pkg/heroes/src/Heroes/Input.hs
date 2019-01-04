{-# LANGUAGE TemplateHaskell #-}
module Heroes.Input (
  Current(..),
  Full(..),
  Key(..),
  MouseButton(..),
  toFull,
  zero
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Key
  = Escape
  | R
  | Left
  | Right
  | N1
  | N2
  | N3

data MouseButton
  = LMB
  | RMB

data Current = Current {
  mouseXY      :: Maybe (Point V2 Int),
  mouseButtons :: MouseButton -> Bool,
  keys         :: Key -> Bool
}

data Full = Full {
  mouseXY      :: Maybe (Point V2 Int),
  mousePressed :: MouseButton -> Bool,
  mouseUp      :: MouseButton -> Bool,
  mouseDown    :: MouseButton -> Bool,
  keyPressed   :: Key -> Bool,
  keyUp        :: Key -> Bool,
  keyDown      :: Key -> Bool,
  quitEvent    :: Bool
}

--------------------------------------------------------------------------------

zero :: Current
zero = Current Nothing (const False) (const False)

toFull :: Current -> Current -> Bool -> Full
toFull (Current _  m1 k1)
       (Current p2 m2 k2)
       quitEvent

  = Full {
      quitEvent,
      mouseXY      = p2,
      mousePressed = m2,
      mouseUp      = mUp,
      mouseDown    = mDown,
      keyPressed   = k2,
      keyUp        = kUp,
      keyDown      = kDown
    }
  where
  mUp   = \b -> m1 b && not (m2 b)
  mDown = \b -> m2 b && not (m1 b)
  kUp   = \b -> k1 b && not (k2 b)
  kDown = \b -> k2 b && not (k1 b)

makeShorthands ''Current
makeShorthands ''Full
