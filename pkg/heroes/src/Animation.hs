module Animation where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype GroupNumber = GroupNumber Int
newtype GroupSize   = GroupSize   Int
newtype FrameNumber = FrameNumber Int

deriving instance Eq GroupNumber
deriving instance Ord GroupNumber
deriving instance Show GroupNumber

deriving instance Eq GroupSize
deriving instance Ord GroupSize
deriving instance Show GroupSize

deriving instance Eq FrameNumber
deriving instance Ord FrameNumber
deriving instance Show FrameNumber

type GroupSizeOf = Handle -> GroupNumber -> GroupSize
