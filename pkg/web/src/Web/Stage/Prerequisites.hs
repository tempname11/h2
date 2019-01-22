{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Web.Stage.Prerequisites () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Essentials                                 (Essentials)
import Stage.Prerequisites
import Web
import Web.Platform ()
import qualified Heroes.Essentials                         as Essentials
import qualified Heroes.FilePath                           as FilePath
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified JavaScript.Web.XMLHttpRequest             as XHR
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
--
instance Prerequisites where
  with _ next = do
    essentials <- loadEssentials
    let cursorResources = () -- XXX
    next $ Prov {..}

loadEssentials :: IO Essentials
loadEssentials = do
  let request = simpleXHR path
      path = FilePath.essentialsBin1
  --
  result <- XHR.contents <$> XHR.xhrByteString request
  case parseWith Essentials.getIt <$> result of
    Nothing -> raise "Couldn't load the Essentials."
    Just (Left str) -> raise ("Meta.parse failure: " <> str)
    Just (Right x) -> return x
