module Common.With where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Prelude
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

-- given some value `a`, performs an IO action.
type Handler a = a -> IO ()

-- suppose that `a` is a value that has meaning only in some circumstances,
-- that is, something needs to be setup up before (and maybe, tore down after).
-- `With a` means "I'll give you an `a`, but only in a controlled environment".
type With a = Handler (Handler a)

-- going deeper: "I'll give you a `With a`, but only in a controlled enviroment".
type With2 a = With (With a)
