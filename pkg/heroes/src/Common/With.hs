module Common.With where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Prelude
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Up a = a -> IO ()
type With2 a = Up (Up a)
type With3 a = Up (Up (Up a))
type With4 a = Up (Up (Up (Up a)))
type With5 a = Up (Up (Up (Up (Up a))))
-- OMG
