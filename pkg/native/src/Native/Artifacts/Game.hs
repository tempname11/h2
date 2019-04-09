{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Artifacts.Game where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Native.GLES ()
import Native.GLX ()
import Heroes.GFX'GLES ()
import Native.SND'SDL ()
import Native.WND'SDL ()
import Native.Platform ()
import Native.Stage.DetermineInput ()
import Native.Stage.Prerequisites ()
import Native.Stage.SystemLibraries ()
import qualified Heroes.Game
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

main' :: IO ()
main' = Heroes.Game.main'
