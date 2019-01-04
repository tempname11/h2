module Heroes.UI.Specials (
  Specials,
  defaults,
  fromOutlineColor
) where

import Heroes
import Heroes.UI
--------------------------------------- ++++++++++++++++++++ -------------------
import qualified Data.Vector.Storable                        as SV
import qualified Data.Vector.Storable.Mutable                as MSV

--------------------------------------------------------------------------------

type Specials = SV.Vector Color

{- Magic Indices:
    0: background
    1: weird shadow outline?
    2: unused?
    3: unused?
    4: shadow bits?
    5: major bits of outline?
    6: minor bits of outline?
    7: minor bits of outline?
-}

defaults :: Specials
defaults = SV.fromList
  [ transparent
  , semitransparent black
  , transparent
  , transparent
  , semitransparent black
  , transparent
  , transparent
  , transparent
  ]

fromOutlineColor :: Color -> Specials
fromOutlineColor c = withOutlineColor c defaults

withOutlineColor :: Color -> Specials -> Specials
withOutlineColor c =
  SV.modify $ \v -> for_ [5, 6, 7] $ \i -> MSV.write v i c

