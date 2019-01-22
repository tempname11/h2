module Heroes.ControlMap where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Heroes.Input                              as Input
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

exit :: Input.Key
exit = Input.Key'Escape

reset :: Input.Key
reset = Input.Key'R

intoPast :: Input.Key
intoPast = Input.Key'Left

intoFuture :: Input.Key
intoFuture = Input.Key'Right
