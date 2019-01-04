{-# LANGUAGE GADTs, Rank2Types, ExplicitForAll #-}
module Lib.Market' where

import Control.Monad ((>=>))

data Market a b c where
  Produce :: b -> Market a b ()
  Consume ::      Market a b a

type MarketF a b  = FFree (Market a b)
produce = liftF . Produce
consume = liftF Consume

trade :: forall a b. MarketF a b () -> [a] -> [b]
trade m xs = reverse $ run xs [] m
  where
  run :: [a] -> [b] -> MarketF a b c -> [b]
  run xs ys m = case m of
    FPure _ -> ys
    FImpure (Produce y) k -> run xs (y : ys) $ k ()
    FImpure (Consume) k ->
      case xs of
        x:xs' ->  run xs' ys $ k x
        [] -> ys

testMarket :: MarketF Int Int ()
testMarket = do
  x <- consume
  y <- consume
  z <- consume
  produce 2
  produce (x + y + z)
  t <- consume
  produce t

-- ! from heroes/src/Commons/FFree ! --
data FFree g a where
  FPure   :: a -> FFree g a
  FImpure :: g x -> (x -> FFree g a) -> FFree g a

instance Functor (FFree g) where
  fmap f (FPure x)     = FPure (f x)
  fmap f (FImpure g k) = FImpure g (fmap f . k)

instance Applicative (FFree g) where
  pure = FPure
  FPure f     <*> x = fmap f x
  FImpure g k <*> x = FImpure g ((<*> x) . k)

instance Monad (FFree g) where
  return = FPure
  FPure x     >>= k  = k x
  FImpure g k >>= k' = FImpure g (k >=> k') -- same as ((>>= k') . k)

liftF :: g a -> FFree g a
liftF fa = FImpure fa FPure

runNat :: Monad s => (forall b. t b -> s b) -> FFree t a -> s a
runNat t m = go m
  where go (FImpure u q) = t u >>= go . q
        go (FPure x) = return x
