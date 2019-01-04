module Common.IO.Misc where 

import Prelude

import Control.Exception.Base           (throwIO)
import Control.Exception.Base           (ErrorCall(ErrorCall))

raise :: String -> IO a
raise = throwIO . ErrorCall
