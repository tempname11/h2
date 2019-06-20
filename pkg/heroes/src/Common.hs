module Common (
  module Common.FFree,
  module Common.Misc,
  module Common.With,
  module Common.Hot,
  module Control.Applicative,
  module Control.Arrow,
  module Control.Lens,
  module Control.Monad,
  module Control.Monad.Fix,
  module Control.Monad.Reader,
  module Control.Monad.State,
  module Control.Monad.Trans,
  module Control.Monad.Writer,
  module Control.Monad.IO.Class,
  module Control.Concurrent.Async,
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
  module Data.Text,
  module Data.Traversable,
  module Data.Word,
  module Debug.Trace,
  module Foreign.C.Types,
  module Foreign.Ptr,
  module Foreign.Storable,
  module Generics.Deriving.Enum,
  module GHC.Exts,
  module GHC.Generics,
  module Linear,
  module Linear.Affine,
  module Prelude,
  module Safe,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common.Fields ()
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common.FFree
import Common.Misc
import Common.With
import Common.Hot                                        (Current(..))
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Applicative                               ((*>))
import Control.Applicative                               ((<$>))
import Control.Applicative                               ((<*))
import Control.Applicative                               ((<*>))
import Control.Applicative                               ((<|>))
import Control.Applicative                               (Applicative)
import Control.Applicative                               (pure)
import Control.Arrow                                     ((>>>))
import Control.Lens                                      ((^.))
import Control.Lens                                      (view)
import Control.Lens                                      ((&))
import Control.Lens                                      (Lens')
import Control.Monad                                     ((<=<))
import Control.Monad                                     ((>=>))
import Control.Monad                                     (join)
import Control.Monad                                     (fail)
import Control.Monad                                     (foldM)
import Control.Monad                                     (guard)
import Control.Monad                                     (replicateM)
import Control.Monad                                     (replicateM_)
import Control.Monad                                     (unless)
import Control.Monad                                     (void)
import Control.Monad                                     (when)
import Control.Monad                                     (zipWithM)
import Control.Monad                                     (zipWithM_)
import Control.Monad.Fix                                 (fix)
import Control.Monad.Reader                              (MonadReader)
import Control.Monad.Reader                              (Reader)
import Control.Monad.Reader                              (ReaderT)
import Control.Monad.Reader                              (ask)
import Control.Monad.Reader                              (runReader)
import Control.Monad.Reader                              (runReaderT)
import Control.Monad.State                               (MonadState)
import Control.Monad.State                               (State)
import Control.Monad.State                               (StateT(StateT))
import Control.Monad.State                               (evalState)
import Control.Monad.State                               (evalStateT)
import Control.Monad.State                               (execState)
import Control.Monad.State                               (execStateT)
import Control.Monad.State                               (runState)
import Control.Monad.State                               (runStateT)
import Control.Monad.State                               (state)
import Control.Monad.Trans                               (lift)
import Control.Monad.Writer                              (MonadWriter)
import Control.Monad.Writer                              (Writer)
import Control.Monad.Writer                              (WriterT)
import Control.Monad.Writer                              (execWriter)
import Control.Monad.Writer                              (execWriterT)
import Control.Monad.Writer                              (runWriter)
import Control.Monad.Writer                              (runWriterT)
import Control.Monad.Writer                              (tell)
import Data.ByteString                                   (ByteString)
import Data.Default.Class                                (Default)
import Data.Default.Class                                (def)
import Data.Either                                       (either)
import Data.Foldable                                     (Foldable)
import Data.Foldable                                     (all)
import Data.Foldable                                     (any)
import Data.Foldable                                     (concat)
import Data.Foldable                                     (foldl')
import Data.Foldable                                     (foldl)
import Data.Foldable                                     (foldr')
import Data.Foldable                                     (foldr)
import Data.Foldable                                     (for_)
import Data.Foldable                                     (mapM_)
import Data.Foldable                                     (maximum)
import Data.Foldable                                     (minimum)
import Data.Foldable                                     (sequence_)
import Data.Foldable                                     (sum)
import Data.Function                                     (on)
import Data.IORef                                        (IORef)
import Data.IORef                                        (modifyIORef)
import Data.IORef                                        (newIORef)
import Data.IORef                                        (readIORef)
import Data.IORef                                        (writeIORef)
import Data.Int                                          (Int16)
import Data.Int                                          (Int32)
import Data.Int                                          (Int64)
import Data.Int                                          (Int8)
import Data.IntMap.Strict                                (IntMap)
import Data.IntSet                                       (IntSet)
import Data.List                                         (length)
import Data.List                                         (map)
import Data.List                                         (partition)
import Data.List                                         (sortBy)
import Data.Map.Strict                                   (Map)
import Data.Maybe                                        (catMaybes)
import Data.Maybe                                        (mapMaybe)
import Data.Maybe                                        (maybe)
import Data.Monoid                                       ((<>))
import Data.Monoid                                       (mappend)
import Data.Monoid                                       (mconcat)
import Data.Monoid                                       (Last(Last))
import Data.Monoid                                       (Monoid)
import Data.Monoid                                       (mempty)
import Data.Ord                                          (comparing)
import Data.Set                                          (Set)
import Data.Text                                         (Text)
import Data.Traversable                                  (Traversable)
import Data.Traversable                                  (for)
import Data.Traversable                                  (mapM)
import Data.Traversable                                  (sequence)
import Data.Word                                         (Word16)
import Data.Word                                         (Word32)
import Data.Word                                         (Word64)
import Data.Word                                         (Word8)
import Debug.Trace                                       (trace)
import Debug.Trace                                       (traceM)
import Debug.Trace                                       (traceShow)
import Debug.Trace                                       (traceShowM)
import Foreign.C.Types                                   (CInt)
import Foreign.Ptr                                       (Ptr)
import Foreign.Storable                                  (Storable)
import GHC.Exts                                          (Double)
import GHC.Generics                                      (Generic)
import Generics.Deriving.Enum                            (GEnum)
import Generics.Deriving.Enum                            (genum)
import Linear                                            (V0 (V0))
import Linear                                            (V1 (V1))
import Linear                                            (V2 (V2))
import Linear                                            (V3 (V3))
import Linear                                            (V4 (V4))
import Linear                                            (lerp)
import Linear.Affine                                     ((.+^))
import Linear.Affine                                     ((.-.))
import Linear.Affine                                     ((.-^))
import Linear.Affine                                     (Point (P))
import Linear                                            (_x)
import Linear                                            (_y)
import Linear                                            (_z)
import Linear                                            (_w)
import Prelude                                           (($))
import Prelude                                           ((&&))
import Prelude                                           ((*))
import Prelude                                           ((+))
import Prelude                                           ((-))
import Prelude                                           ((.))
import Prelude                                           ((/))
import Prelude                                           ((/=))
import Prelude                                           ((<))
import Prelude                                           ((<=))
import Prelude                                           ((=<<))
import Prelude                                           ((==))
import Prelude                                           ((>))
import Prelude                                           ((>=))
import Prelude                                           ((>>))
import Prelude                                           ((>>=))
import Prelude                                           ((^))
import Prelude                                           ((||))
import Prelude                                           (Bool (True, False))
import Prelude                                           (Bounded)
import Prelude                                           (Double)
import Prelude                                           (Either (Left, Right))
import Prelude                                           (Enum)
import Prelude                                           (Eq)
import Prelude                                           (Float)
import Prelude                                           (Fractional)
import Prelude                                           (Functor)
import Prelude                                           (IO)
import Prelude                                           (Int)
import Prelude                                           (Integer)
import Prelude                                           (Integral)
import Prelude                                           (Maybe (Just, Nothing))
import Prelude                                           (Monad)
import Prelude                                           (Num)
import Prelude                                           (Ord)
import Prelude                                           (Ordering (EQ, LT, GT))
import Prelude                                           (Show)
import Prelude                                           (String)
import Prelude                                           (abs)
import Prelude                                           (atan2)
import Prelude                                           (ceiling)
import Prelude                                           (compare)
import Prelude                                           (const)
import Prelude                                           (curry)
import Prelude                                           (div)
import Prelude                                           (divMod)
import Prelude                                           (error)
import Prelude                                           (flip)
import Prelude                                           (floor)
import Prelude                                           (fmap)
import Prelude                                           (filter)
import Prelude                                           (fst)
import Prelude                                           (id)
import Prelude                                           (lines)
import Prelude                                           (log)
import Prelude                                           (max)
import Prelude                                           (maxBound)
import Prelude                                           (maybe)
import Prelude                                           (min)
import Prelude                                           (minBound)
import Prelude                                           (mod)
import Prelude                                           (negate)
import Prelude                                           (not)
import Prelude                                           (null)
import Prelude                                           (otherwise)
import Prelude                                           (pi)
import Prelude                                           (print)
import Prelude                                           (putStrLn)
import Prelude                                           (return)
import Prelude                                           (reverse)
import Prelude                                           (round)
import Prelude                                           (seq)
import Prelude                                           (show)
import Prelude                                           (snd)
import Prelude                                           (sqrt)
import Prelude                                           (subtract)
import Prelude                                           (uncurry)
import Prelude                                           (undefined)
import Prelude                                           (unlines)
import Prelude                                           (unwords)
import Prelude                                           (words)
import Prelude                                           (zip)
import Prelude                                           (zipWith)
import Safe                                              (headMay)
import Control.Monad.IO.Class                            (liftIO)
import Control.Concurrent.Async                          (async)
import Control.Concurrent.Async                          (Async)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
