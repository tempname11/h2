module Native.UI.Cursor (
  lengths,
  stuff
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import qualified Heroes.FilePath                           as FilePath
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

lengths :: Platform => V.Vector Int
lengths = V.fromList . map length . concat . map fst $ stuff

-- XXX rename, restructure
stuff :: Platform => [([[Point V2 CInt]], String)]
stuff = map (\(s, i) -> ((map . map) P i, s))
  [ (FilePath.h3 <> "Defs/CRCOMBAT.def",
     [ [V2  0  0]
     , [V2  6 13]
     , [V2 12 13]
     , [V2 11 11]
     , [V2 13  9]
     , [V2  6  3]
     , [V2 20  1]
     , [V2 31  6]
     , [V2 21 21]
     , [V2  6 31]
     , [V2  0 22]
     , [V2  0  5]
     , [V2  0  1]
     , [V2 13 11]
     , [V2 13  2]
     , [V2 12  8]
     , [V2 12  9]
     , [V2 12 12]
     , [V2 13 15]
     , [V2 14 10]
     ])
  , (FilePath.h3 <> "Defs/crdeflt.def",
     [ [V2  0  0] -- unused
     , [V2  0  0] -- unused
     , [V2  9 11]
     ])
  , (FilePath.h3 <> "Defs/Crspell.def",
     [[ V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
      , V2 16 15
     ]])
  ]
