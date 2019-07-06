{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Reflex.Jumpstart (
  E,
  B,
  Network(..),
  extern,
  fire
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Applicative
import Control.Monad
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
import System.IO.Unsafe                                  (unsafePerformIO)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Gx = R.Global
type Gt = R.SpiderTimeline R.Global

newtype T x = T {
  unT :: IORef (Maybe (RH.EventTrigger Gt x))
}

data F = forall x. F (T x) x

newtype E x = E { unE :: (R.Event Gt x) }
  deriving (Functor)

newtype B x = B { unB :: (R.Behavior Gt x) }
  deriving (Functor, Applicative, Monad)

instance Applicative E where
  pure x = const x <$> frame
  (<*>) (E f) (E e) = E $ R.coincidence (fmap (($ e) . fmap) f)

instance Monad E where
  (>>=) (E e) f = E $ R.coincidence (unE <$> fmap f e)

instance Alternative E where
  empty = E R.never
  (<|>) (E l) (E r) = E (R.leftmost [l, r])

class Monad m => Network m where
  sample :: B x -> m x
  hold :: x -> E x -> m (B x)
  -- XXX
  holdFix :: x -> (B x -> (E x, y)) -> m (B x, y)
  affect :: IO x -> m x

instance Network IO where
  sample (B b) = RS.runSpiderHost $ R.sample b
  hold x (E e) = fmap B $ RS.runSpiderHost $ R.hold x e
  -- XXX
  holdFix x f = RS.runSpiderHost $ mdo
    let (E e, y) = f (B b)
    rec b <- R.hold x e
    return (B b, y)
  affect m = m

instance Network E where
  sample (B b) = E $ R.pushAlways (\_ -> R.sample b) (unE frame)
  hold x (E e) = E . (fmap B) $ R.pushAlways (\_ -> R.hold x e) (unE frame)
  -- XXX
  holdFix x f = E $ R.pushAlways
    (\_ -> mdo
      let (E e, y) = f (B b)
      rec b <- R.hold x e
      return (B b, y)
    )
    (unE frame)
  affect m = E $ R.pushAlways (\_ -> liftIO m) (unE frame)

frameTuple :: (E (), () -> F, RH.EventHandle Gt ())
frameTuple = unsafePerformIO $ RS.runSpiderHost $ do
  (e, r) <- RH.newEventWithTriggerRef
  h <- RH.subscribeEvent e
  return (E e, \x -> F (T r) x, h)

frame :: E ()
frame = (\(e, _, _) -> e) frameTuple

frameT :: () -> F
frameT = (\(_, t, _) -> t) frameTuple

frameSubHandle :: RH.EventHandle Gt ()
frameSubHandle = (\(_, _, h) -> h) frameTuple

extern :: IO (E x, x -> F)
extern = RS.runSpiderHost $ do
  (e, r) <- RH.newEventWithTriggerRef
  return (E e, \x -> F (T r) x)

fire :: [F] -> IO ()
fire fs =
  seq frameSubHandle $
  RS.runSpiderHost $
  do
    let
      fs' = (frameT () : fs)
    --
    events <- for fs' $ \(F (T r) x) -> do
      mt <- liftIO $ readIORef r
      return $ case mt of
        Just t -> Just (t :=> pure x)
        Nothing -> Nothing
    RH.fireEvents (catMaybes events) 
