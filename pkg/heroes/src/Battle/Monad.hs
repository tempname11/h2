module Battle.Monad where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Battle
import Battle.Setup
import qualified Battle.AM                                 as AM
import qualified Battle.PM                                 as PM
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type P =
  StateT Battle (
    WriterT (Last PM.Marker, [AM.Marker]) (
      ReaderT Setup (
        Either Failure
      )
    )
  )

type P0 = P ()
