{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Stage.Prerequisites () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Stage.Prerequisites
import qualified Heroes.Essentials                         as Essentials
import qualified Heroes.FilePath                           as FilePath
import qualified Native.ResourceIO                         as Resource
import qualified Native.UI.Cursor                          as Cursor
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.ByteString                           as B
import qualified Data.Vector                               as V
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Prerequisites where
  with _ next = do
    putStrLn $ "Loading essentials..."
    buf <- B.readFile FilePath.essentialsBin1
    essentials <- case parseWith Essentials.getIt buf of
      Left str -> raise ("Meta.parse failure: " <> str)
      Right m -> return m
    --
    --
    cursorResources <-
      V.concat <$> for Cursor.stuff (uncurry Resource.loadCursor)
    --
    result <- next $ Prov {..}
    --
    (mapM_ . mapM_) SDL.freeCursor cursorResources
    return result
