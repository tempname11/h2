module Heroes.Plan.Animation where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Plan.Types
import Animation
import Animation.Scene
import Animation.Command
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

setGroupNumber :: Handle -> GroupNumber -> Offset -> M0
setGroupNumber h g = append (HC h $ SetGroupNumber g)

setFacing :: Handle -> Facing -> Offset -> M0
setFacing h f = append (HC h $ SetFacing f)

setPosition :: Handle -> Position -> Offset -> M0
setPosition h p = append (HC h $ SetPosition p)

setHeight :: Handle -> CInt -> Offset -> M0
setHeight h p = append (HC h $ SetHeight p)

setAnimated :: Handle -> Bool -> Offset -> M0
setAnimated h b = append (HC h $ SetAnimated b)

add :: Handle -> Actor -> Offset -> M0
add h a = append (HC h $ Add a)

remove :: Handle -> Offset -> M0
remove h = append (HC h $ Remove)

append :: Command -> Offset -> M0
append c o = tell [(o, Left c)]
