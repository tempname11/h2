{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Reflex.Jumpstart (
  E,
  B,
  Trigger,
  Runtime,
  Firing,
  gate,
  hold,
  sample,
  switch,
  coincidence,
  subscribe,
  newEvent,
  run,
  fire,
  (==>)
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Monad.IO.Class
import Data.Dependent.Sum                                (DSum((:=>)))
import Data.Maybe
import Data.Traversable
import Data.IORef
import Prelude
import qualified Reflex                                    as R
import qualified Reflex.Host.Class                         as RH
import qualified Reflex.Spider                             as RS
import qualified Reflex.Spider.Internal                    as RI
import qualified Reflex.Spider.Addenum                     as RA
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Gx = R.Global
type Gt = R.SpiderTimeline R.Global

newtype Runtime x = Runtime {
  unRuntime :: RI.EventM Gx x
} deriving (Functor, Applicative, Monad, MonadIO)

newtype Trigger x = Trigger {
  unTrigger :: IORef (Maybe (RH.EventTrigger Gt x))
}

data Firing = forall x. Firing (Trigger x) x

(==>) :: Trigger x -> x -> Firing
(==>) = Firing

newtype E x = E { unE :: (R.Event Gt x) }
  deriving (Functor)

newtype B x = B { unB :: (R.Behavior Gt x) }
  deriving (Functor, Applicative, Monad)

instance Monoid (E x) where
  mempty = E R.never
  mappend (E l) (E r) = E (R.leftmost [l, r])

gate :: B Bool -> E x -> E x
gate (B b) (E e) = E $ R.gate b e

hold :: x -> E x -> Runtime (B x)
hold x (E e) = Runtime $ B <$> R.hold x e

sample :: B x -> Runtime x
sample (B b) = Runtime $ R.sample b

switch :: B (E x) -> E x
switch (B b) = E $ R.switch (fmap unE b)

coincidence :: E (E x) -> E x
coincidence (E e) = E $ R.coincidence (fmap unE e)

subscribe :: E x -> (x -> Runtime y) -> Runtime (E y)
subscribe (E e) f =
  Runtime $
    E . RI.SpiderEvent <$>
      RA.subscribeEffect @Gx
        (RI.unSpiderEvent e)
        (unRuntime . f)

newEvent :: IO (E x, Trigger x)
newEvent = RS.runSpiderHost $ do
  (e, r) <- RH.newEventWithTriggerRef
  return (E e, Trigger r)

fire :: [Firing] -> IO ()
fire fs = RS.runSpiderHost $ do
  events <- for fs $ \(Firing (Trigger r) x) -> do
    mt <- liftIO $ readIORef r
    return $ case mt of
      Just t -> Just (t :=> pure x)
      Nothing -> Nothing
  --
  RH.fireEvents (catMaybes events)

run :: Runtime x -> IO x
run (Runtime m) =
  RS.runSpiderHost $
    RI.runFrame m
