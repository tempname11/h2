{-# LANGUAGE Rank2Types #-}
module Battle.Monad.Utils where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Battle
import Battle.Setup
import Battle.Monad
import qualified Battle.AM                                 as AM
import qualified Battle.PM                                 as PM
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Lens                                      (Lens')
import Control.Monad.State                               (get)
import Control.Monad.State                               (put)
import Extra.Field.Optics.Internal                       (Binoculars')
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

-- mnemonic:
-- # for battle
-- . for result
-- ? for Maybe
-- ! for state
-- * for path markers
-- & for anim markers

(#?.) :: (Setup, Battle) -> P a -> Maybe a
(#?.) (setup, battle) p =
  rightIsJust $ fmap fst $ runReaderT (runWriterT $ evalStateT p battle) setup

(#?.!) :: (Setup, Battle) -> P a -> Maybe (a, Battle)
(#?.!) (setup, battle) p =
  rightIsJust $ fmap fst $ runReaderT (runWriterT $ runStateT p battle) setup

(#?!) :: (Setup, Battle) -> P a -> Maybe Battle
(#?!) (setup, battle) p =
  rightIsJust $ fmap fst $ runReaderT (runWriterT $ execStateT p battle) setup

(#%!*.) :: (Setup, Battle) -> P a -> Either Failure (Battle, Last PM.Marker, a)
(#%!*.) = helper fst

(#%!&.) :: (Setup, Battle) -> P a -> Either Failure (Battle, [AM.Marker], a)
(#%!&.) = helper snd

helper ::
  ((Last PM.Marker, [AM.Marker]) -> b) ->
  (Setup, Battle) ->
  P a ->
  Either Failure (Battle, b, a)
helper f (setup, battle) p =
  fmap xform $ runReaderT (runWriterT $ runStateT p battle) setup
  where
  xform ((a, b), e) = (b, f e, a)

--------------------------------------------------------------------------------

type B a = Binoculars' Battle a
type L a = Lens' Battle a

infix 4 -=!
infix 4 +=!
infix 4 .=!
infix 4 %=!

(-=!) :: (Num a) => B a -> a -> P0
(-=!) l a = (%=!) l (\x -> x - a)

(+=!) :: (Num a) => B a -> a -> P0
(+=!) l a = (%=!) l (\x -> x + a)

(%=!) :: B a -> (a -> a) -> P0
(%=!) l f = do
  v <- get
  case overMay l f v of
    Just v' -> put v'
    Nothing -> catastrophic

(.=!) :: B a -> a -> P0
(.=!) l a = do
  v <- get
  case setMay l a v of
    Just v' -> put v'
    Nothing -> catastrophic

(?!) :: B a -> P a
(?!) l = do
  v <- get
  case viewMay l v of
    Just x -> return x
    Nothing -> catastrophic

(?) :: L a -> P a
(?) = use

(?-) :: Lens' Setup a -> P a
(?-) l = fmap (view l) ask

(?!-) :: Binoculars' Setup a -> P a
(?!-) l = do
  v <- ask
  case viewMay l v of
    Just x -> return x
    Nothing -> catastrophic

catastrophic :: P a
catastrophic = lift . lift . lift $ Left Catastrophic

invalid :: P a
invalid = lift . lift . lift $ Left Invalid

enforce :: Bool -> P0
enforce = flip when invalid . not

enforceM :: P Bool -> P0
enforceM = (>>= enforce)

pMark :: PM.Marker -> P0
pMark marker = tell (Last (Just marker), [])

aMark :: AM.Marker -> P0
aMark marker = tell (Last Nothing, [marker])
