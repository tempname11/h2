module Heroes.Root.BattleScreen.ControlMap where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Heroes.Input                              as Input
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

start :: Input.Key
start = Input.Key'Enter

exit :: Input.Key
exit = Input.Key'Escape

reset :: Input.Key
reset = Input.Key'R

intoPast :: Input.Key
intoPast = Input.Key'Left

intoFuture :: Input.Key
intoFuture = Input.Key'Right
