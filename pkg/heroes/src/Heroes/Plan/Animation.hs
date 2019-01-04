module Heroes.Plan.Animation where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Plan.Types
import Animation
import Animation.Scene
import Animation.Command
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

setGroupNumber :: Handle -> GroupNumber -> Offset -> S0
setGroupNumber h g = append $ HC h $ SetGroupNumber g

setFacing :: Handle -> Facing -> Offset -> S0
setFacing h f = append $ HC h $ SetFacing f

setPosition :: Handle -> Position -> Offset -> S0
setPosition h p = append $ HC h $ SetPosition p

setHeight :: Handle -> CInt -> Offset -> S0
setHeight h p = append $ HC h $ SetHeight p

setAnimated :: Handle -> Bool -> Offset -> S0
setAnimated h b = append $ HC h $ SetAnimated b

add :: Handle -> Actor -> Offset -> S0
add h a = append $ HC h $ Add a

remove :: Handle -> Offset -> S0
remove h = append $ HC h $ Remove

append :: Command -> Offset -> S0
append ac (Offset i) = at i %= f
  where
  f Nothing = Just ([ac], [])
  f (Just (acs, scs)) = Just (ac : acs, scs)
