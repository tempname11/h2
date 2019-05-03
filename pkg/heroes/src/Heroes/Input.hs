module Heroes.Input (
  Snapshot(..),
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
  = Key'Escape
  | Key'Enter
  | Key'R
  | Key'Left
  | Key'Right
  | Key'1
  | Key'2
  | Key'3

data MouseButton
  = LMB
  | RMB

data Snapshot = Snapshot {
  mouseAt :: Maybe (Point V2 Int),
  mouseButtons :: MouseButton -> Bool,
  keys :: Key -> Bool
} deriving (Generic)

data Full = Full {
  mouseAt :: Maybe (Point V2 Int),
  mousePressed :: MouseButton -> Bool,
  mouseUp :: MouseButton -> Bool,
  mouseDown :: MouseButton -> Bool,
  keyPressed :: Key -> Bool,
  keyUp :: Key -> Bool,
  keyDown :: Key -> Bool,
  quitEvent :: Bool
} deriving (Generic)

--------------------------------------------------------------------------------

zero :: Snapshot
zero = Snapshot Nothing (const False) (const False)

toFull :: Snapshot -> Snapshot -> Bool -> Full
toFull
  (Snapshot _  m1 k1)
  (Snapshot p2 m2 k2)
  quitEvent
  = Full {
      quitEvent,
      mouseAt = p2,
      mousePressed = m2,
      mouseUp = mUp,
      mouseDown = mDw,
      keyPressed = k2,
      keyUp = kUp,
      keyDown = kDw
    }
  where
  mUp = \b -> m1 b && not (m2 b)
  mDw = \b -> m2 b && not (m1 b)
  kUp = \b -> k1 b && not (k2 b)
  kDw = \b -> k2 b && not (k1 b)
