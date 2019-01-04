module Web.API (
  forkPreferred
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common.IO
import Control.Concurrent                                (forkIO)
import Control.Concurrent                                (ThreadId)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

forkPreferred :: IO () -> IO ThreadId
forkPreferred = forkIO
