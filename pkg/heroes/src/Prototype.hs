{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Prototype where

import Common
import Reflex.Jumpstart
import Control.Monad.IO.Class (liftIO)
import Prelude

main :: IO ()
main = do
  (e, t) <- extern
  b <- hold (0 :: Int) e
  fire [t 42]
  let
    b2 = do
      void b
      affect $ print (101 :: Int)
      return (102 :: Int)
    b3 = do
      x <- b2
      y <- b2
      x' <- join $ hold x empty
      return $ x' + y
  sample b3 >>= print
  sample b3 >>= print
  --
