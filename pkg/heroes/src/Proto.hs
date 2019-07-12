module Proto where

import Common
import Control.Concurrent
import Reflex.Jumpstart

latest :: E (E x, Int) -> IO (E x)
latest e = do
  b <- hold (empty, 0) $ do
    v <- e
    affect $ print "latest e fired"
    return v
  return $ do
    (v, i) <- sample b
    affect $ print "sample b!?"
    affect $ print i
    vv <- v -- BUG!?: does not trigger??
    affect $ print "vv!?"
    return vv

proto :: IO ()
proto = do
  (e, f) <- extern
  el <- latest $ do
    e
    (ex, fx) <- extern 
    void $ affect $ do
      forkIO $ do
        -- print "thread spawn"
        threadDelay $ 4 * 1000000
        print "thread delay ended"
        fire [fx 42]
        print "thread exit"
    return (
        (do
          v <- ex
          affect $ print "ex fired"
          return v
        ),
        111
      )

  b <- hold (0 :: Int) $ do
    v <- el
    affect $ print "el fired"
    return v
  fire [f ()]
  threadDelay $ 10 * 1000000
  print "delay ended"
  sample b >>= print
  print "exit"
