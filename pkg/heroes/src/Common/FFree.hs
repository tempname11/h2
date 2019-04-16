{-# LANGUAGE RankNTypes #-}
-- XXX remove, it's slow anyway?
module Common.FFree (
  liftF,
  runNat,
  FFree
) where

import Prelude
import Control.Monad ((>=>))

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
