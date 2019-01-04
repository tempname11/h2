module Heroes.FilePath where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import qualified Platform.Config                           as Config
import Platform.Config                                   (Config)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Path = String

h3 :: String
h3 = "h3-assets/"

prod :: Config => String
prod = Config.productionPrefix

essentialsBin1 :: Config => Path
essentialsBin1 = prod <> "essentials.bin1"

music :: Config => Path
music = prod <> "silent.wav"

background :: Config => Path
background = prod <> "bg" <> Config.staticSpriteExtension

cellShaded :: Config => Path
cellShaded = prod <> "cell-shaded" <> Config.staticSpriteExtension

cellOutline :: Config => Path
cellOutline = prod <> "cell-outline" <> Config.staticSpriteExtension

-- XXX "stringly-typed"
pngPathOf :: Config => String -> String
pngPathOf defName = prod <> defName <> ".png"

staticPathOf :: Config => String -> String
staticPathOf defName = prod <> defName <> Config.staticSpriteExtension
