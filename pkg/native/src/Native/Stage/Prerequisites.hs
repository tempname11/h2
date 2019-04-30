{-# OPTIONS_GHC -Wno-orphans #-}
module Native.Stage.Prerequisites () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Stage.Prerequisites
import qualified Heroes.Essentials                         as Essentials
import qualified Heroes.FilePath                           as FilePath
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.ByteString                           as B
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance Prerequisites where
  with _ next = do
    putStrLn $ "Loading essentials..."
    buf <- B.readFile FilePath.essentialsBin1
    --
    essentials <- case parseWith Essentials.get buf of
      Left str -> raise ("Meta.parse failure: " <> str)
      Right m -> return m
    --
    next $ Prov {..}
