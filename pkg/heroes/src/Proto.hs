module Proto where

import Common
import Reflex.Jumpstart

main' :: IO ()
main' = do
  (e, f) <- extern @Int
  b <- hold (0 :: Int) $ do
    i <- e
    affect $ print True
    return (i * i)
  fire [f 16]
  sample b >>= print
