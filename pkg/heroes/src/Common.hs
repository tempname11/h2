module Common (
  module Common.FFree,
  module Common.Misc,
  module Common.With,
  module Control.Applicative,
  module Control.Arrow,
  module Control.Lens,
  module Control.Monad,
  module Control.Monad.Fix,
  module Control.Monad.Reader,
  module Control.Monad.State,
  module Control.Monad.Trans,
  module Control.Monad.Writer,
  module Data.ByteString,
  module Data.Default.Class,
  module Data.Either,
  module Data.Foldable,
  module Data.Function,
  module Data.IORef,
  module Data.Int,
  module Data.IntMap.Strict,
  module Data.IntSet,
  module Data.List,
  module Data.Map.Strict,
  module Data.Maybe,
  module Data.Monoid,
  module Data.Ord,
  module Data.Set,
  module Data.Traversable,
  module Data.Word,
  module Debug.Trace,
  module Foreign.C.Types,
  module Foreign.Ptr,
  module GHC.Exts,
  module Linear,
  module Linear.Affine,
  module Prelude,
  module Safe,
) where

import Common.FFree
import Common.Misc
import Common.With

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Prelude                                           ((.))
import Prelude                                           (($))
import Prelude                                           (flip)
import Prelude                                           (fst)
import Prelude                                           (snd)
import Prelude                                           ((/=))
import Prelude                                           ((==))
import Prelude                                           (not)
import Prelude                                           (Maybe (Just, Nothing))
import Prelude                                           (Bool (True, False))
import Prelude                                           (Ordering (EQ, LT, GT))
import Prelude                                           ((<))
import Prelude                                           ((>))
import Prelude                                           ((<=))
import Prelude                                           ((>=))
import Prelude                                           (Bounded)
import Prelude                                           (minBound)
import Prelude                                           (maxBound)
import Prelude                                           (Int)
import Prelude                                           (Integer)
import Prelude                                           (Float)
import Prelude                                           (Double)
import Prelude                                           ((+))
import Prelude                                           ((-))
import Prelude                                           ((*))
import Prelude                                           ((/))
import Prelude                                           ((^))
import Prelude                                           (negate)
import Prelude                                           (subtract)
import Prelude                                           (div)
import Prelude                                           (mod)
import Prelude                                           (abs)
import Prelude                                           (log)
import Prelude                                           (sqrt)
import Prelude                                           (pi)
import Prelude                                           (atan2)
import Prelude                                           (divMod)
import Prelude                                           ((||))
import Prelude                                           ((&&))
import Prelude                                           (round)
import Prelude                                           (floor)
import Prelude                                           (ceiling)
import Prelude                                           (max)
import Prelude                                           (min)
import Prelude                                           (compare)
import Prelude                                           (maybe)
import Prelude                                           (Either (Left, Right))
import Prelude                                           (error)
import Prelude                                           (return)
import Prelude                                           (String)
import Prelude                                           (Show)
import Prelude                                           (show)
import Prelude                                           (Functor)
import Prelude                                           (Monad)
import Prelude                                           (fmap)
import Prelude                                           (id)
import Prelude                                           (const)
import Prelude                                           (curry)
import Prelude                                           (uncurry)
import Prelude                                           (reverse)
import Prelude                                           (null)
import Prelude                                           ((>>))
import Prelude                                           ((>>=))
import Prelude                                           ((=<<))
import Prelude                                           (zip)
import Prelude                                           (zipWith)
import Prelude                                           (Num)
import Prelude                                           (Integral)
import Prelude                                           (Fractional)
import Prelude                                           (Eq)
import Prelude                                           (Ord)
import Prelude                                           (Enum)
import Prelude                                           (otherwise)
import Prelude                                           (lines)
import Prelude                                           (unlines)
import Prelude                                           (words)
import Prelude                                           (unwords)

import Data.Function                                     (on)

import Data.Foldable                                     (concat)
import Data.Foldable                                     (mapM_)
import Data.Foldable                                     (maximum)
import Data.Foldable                                     (minimum)
import Data.Foldable                                     (foldl)
import Data.Foldable                                     (foldl')
import Data.Foldable                                     (foldr)
import Data.Foldable                                     (foldr')
import Data.Foldable                                     (for_)
import Data.Foldable                                     (any)
import Data.Foldable                                     (all)
import Data.Foldable                                     (sum)
import Data.Foldable                                     (sequence_)
import Data.Foldable                                     (Foldable)

import Data.Traversable                                  (Traversable)
import Data.Traversable                                  (for)
import Data.Traversable                                  (sequence)
import Data.Traversable                                  (mapM)

import Control.Applicative                               (Applicative)
import Control.Applicative                               ((<$>))
import Control.Applicative                               ((<*>))
import Control.Applicative                               ((<*))
import Control.Applicative                               ((*>))
import Control.Applicative                               ((<|>))

import Control.Monad                                     ((>=>))
import Control.Monad                                     ((<=<))
import Control.Monad                                     (fail)
import Control.Monad                                     (zipWithM)
import Control.Monad                                     (zipWithM_)
import Control.Monad                                     (when)
import Control.Monad                                     (unless)
import Control.Monad                                     (foldM)
import Control.Monad                                     (join)
import Control.Monad                                     (guard)
import Control.Monad                                     (void)
import Control.Monad                                     (replicateM)
import Control.Monad                                     (replicateM_)

import Control.Monad.Fix                                 (fix)

import Control.Monad.Trans                               (lift)

import Control.Monad.State                               (State)
import Control.Monad.State                               (state)
import Control.Monad.State                               (StateT(StateT))
import Control.Monad.State                               (MonadState)
import Control.Monad.State                               (runState)
import Control.Monad.State                               (runStateT)
import Control.Monad.State                               (execState)
import Control.Monad.State                               (execStateT)
import Control.Monad.State                               (evalState)
import Control.Monad.State                               (evalStateT)
import Control.Monad.State                               (get)
import Control.Monad.State                               (put)

import Control.Monad.Reader                              (ask)
import Control.Monad.Reader                              (Reader)
import Control.Monad.Reader                              (ReaderT)
import Control.Monad.Reader                              (MonadReader)
import Control.Monad.Reader                              (runReader)
import Control.Monad.Reader                              (runReaderT)

import Control.Monad.Writer                              (Writer)
import Control.Monad.Writer                              (WriterT)
import Control.Monad.Writer                              (MonadWriter)
import Control.Monad.Writer                              (execWriter)
import Control.Monad.Writer                              (execWriterT)
import Control.Monad.Writer                              (runWriter)
import Control.Monad.Writer                              (runWriterT)
import Control.Monad.Writer                              (tell)

import Data.Maybe                                        (mapMaybe)
import Data.Maybe                                        (maybe)
import Data.Maybe                                        (catMaybes)

import Linear.Affine                                     ((.+^))
import Linear.Affine                                     ((.-^))
import Linear.Affine                                     ((.-.))
import Linear.Affine                                     (Point (P))

import Linear                                            (lerp)
import Linear                                            (V0 (V0))
import Linear                                            (V1 (V1))
import Linear                                            (V2 (V2))
import Linear                                            (V3 (V3))
import Linear                                            (V4 (V4))

import Data.List                                         (map)
import Data.List                                         (length)
import Data.List                                         (partition)
import Data.List                                         (sortBy)

import Data.Monoid                                       (Last(Last))
import Data.Monoid                                       ((<>))

import Debug.Trace                                       (trace)
import Debug.Trace                                       (traceM)
import Debug.Trace                                       (traceShow)
import Debug.Trace                                       (traceShowM)

import Data.Word                                         (Word8)
import Data.Word                                         (Word16)
import Data.Word                                         (Word32)
import Data.Word                                         (Word64)

import Data.Int                                          (Int8)
import Data.Int                                          (Int16)
import Data.Int                                          (Int32)
import Data.Int                                          (Int64)

import Control.Lens                                      ((&))
import Control.Arrow                                     ((>>>))
import Data.Map.Strict                                   (Map)
import Data.Set                                           (Set)
import Data.Either                                       (either)
import Safe                                              (headMay)
import Data.IntMap.Strict                                (IntMap)
import Data.IntSet                                       (IntSet)
import Data.Ord                                          (comparing)
import Foreign.C.Types                                   (CInt)
import Data.ByteString                                   (ByteString)
import GHC.Exts                                          (Double)
import Data.Default.Class                                (Default)
import Data.Default.Class                                (def)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
--
import Prelude                                           (IO)
import Prelude                                           (print)
import Prelude                                           (putStrLn)
import Data.IORef                                        (IORef)
import Data.IORef                                        (newIORef)
import Data.IORef                                        (readIORef)
import Data.IORef                                        (writeIORef)
import Foreign.Ptr                                       (Ptr)
