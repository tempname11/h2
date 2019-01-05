module Native.API (
  forkPreferred
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
import Control.Concurrent                                (forkOS)
import Control.Concurrent                                (ThreadId)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

forkPreferred :: IO () -> IO ThreadId
forkPreferred = forkOS
