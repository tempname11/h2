module Heroes.ControlMap where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Heroes.Input                              as Input
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

exit :: Input.Key
exit = Input.Escape

reset :: Input.Key
reset = Input.R

intoPast :: Input.Key
intoPast = Input.Left

intoFuture :: Input.Key
intoFuture = Input.Right
