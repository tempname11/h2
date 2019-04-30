module Heroes.FilePath where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Font                                       (Font)
import Heroes.Font                                       (fontNameOf)
import qualified Heroes.Platform                           as Platform
import Heroes.Platform                                   (Platform)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Path = String

prod :: Platform => String
prod = Platform.productionPrefix

essentialsBin1 :: Platform => Path
essentialsBin1 = prod <> "essentials.bin1"

music :: Platform => Path
music = prod <> "silent.wav"

background :: Platform => Path
background = prod <> "bg" <> ".png"

cellShaded :: Platform => Path
cellShaded = prod <> "cell-shaded" <> ".png"

cellOutline :: Platform => Path
cellOutline = prod <> "cell-outline" <> ".png"

fontAtlasPathOf :: Platform => Font -> String
fontAtlasPathOf font = prod <> fontNameOf font <> ".png"

-- XXX "stringly-typed"
pngPathOf :: Platform => String -> String
pngPathOf defName = prod <> defName <> ".png"

staticPathOf :: Platform => String -> String
staticPathOf defName = prod <> defName <> ".png"

soundPathOf :: Platform => String -> String
soundPathOf wavName = prod <> "Sounds/" <> wavName <> ".wav"

cursorPathOf :: Platform => String -> String
cursorPathOf curName = prod <> "Cursors/" <> curName <> ".png"
