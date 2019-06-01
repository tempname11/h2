{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Prototype2 where

import Common
import Reflex.Jumpstart
import Control.Monad.IO.Class (liftIO)
import Prelude

main :: IO ()
main = do
  (e, t) <- newEvent
  b <- run $ hold (0 :: Int) e
  fire [t ==> 42]
  run (sample b) >>= print
