module Common.Misc where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Foreign.Ptr
import Foreign.C.Types

import Prelude                                             as P
import qualified Data.IntMap.Strict                        as I
import qualified Data.Map.Strict                           as M
import qualified Data.Set                                  as S
import qualified Data.Vector                               as V
import qualified Data.Vector.Storable                      as SV

import Control.Monad.State                               (StateT(StateT))
import Control.Monad.State                               (state)

import Data.Binary.Get                                   (Get)
import Data.Binary.Get                                   (pushChunk)
import Data.Binary.Get                                   (Decoder(..))
import Data.Binary.Get                                   (pushEndOfInput)
import Data.Binary.Get                                   (runGetIncremental)

import Data.Binary.Put                                   (Put)
import Data.Binary.Put                                   (runPut)

import Debug.Trace                                       (traceShow)
import Data.ByteString                                   (ByteString)
import Data.ByteString.Lazy                              (toStrict)
import Data.Monoid                                       ((<>))
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

-- XXX use a library definition?
data Stream a = Cons a (Stream a)

nextS :: Stream a -> Stream a
nextS (Cons _ s) = s

currentS :: Stream a -> a
currentS (Cons a _) = a

rightIsJust :: Either e a -> Maybe a
rightIsJust (Left _) = Nothing
rightIsJust (Right x) = Just x

justIsTrue :: Maybe a -> Bool
justIsTrue (Just _) = True
justIsTrue (Nothing) = False

collectJustsM :: Monad m => m (Maybe a) -> m [a]
collectJustsM m = loop []
  where loop xs = do
          result <- m
          case result of
            Nothing -> return xs
            Just x  -> loop (x:xs)

generateM_ :: Monad m => Int -> (Int -> m a) -> m ()
generateM_ n g = sequence_ $ map g [0..n - 1]

dot :: (b -> c) -> (a -> b) -> (a -> c)
dot = (.)

dot2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dot2 = (.) . (.)

fmap2 :: (Functor f1, Functor f2) =>
         (a -> b) -> f2 (f1 a) -> f2 (f1 b)
fmap2 = fmap . fmap

fmap3 :: (Functor f1, Functor f2, Functor f3) =>
         (a -> b) -> f3 (f2 (f1 a)) -> f3 (f2 (f1 b))
fmap3 = fmap . fmap . fmap

(<<$>>) :: (Functor f1, Functor f2) =>
           (a -> b) -> f2 (f1 a) -> f2 (f1 b)
(<<$>>) = fmap2

(<<<$>>>) :: (Functor f1, Functor f2, Functor f3) =>
             (a -> b) -> f3 (f2 (f1 a)) -> f3 (f2 (f1 b))
(<<<$>>>) = fmap3

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

(<<&>>) :: (Functor f1, Functor f2) =>
           f2 (f1 a) -> (a -> b) -> f2 (f1 b)
(<<&>>) = flip fmap2

(<<<&>>>) :: (Functor f1, Functor f2, Functor f3) =>
             f3 (f2 (f1 a)) -> (a -> b) -> f3 (f2 (f1 b))
(<<<&>>>) = flip fmap3

infixl 5 <&>
infixl 4 <<$>>
infixl 5 <<&>>
infixl 4 <<<$>>>
infixl 5 <<<&>>>

presumeJust :: String -> Maybe a -> a
presumeJust explanation m = case m of
  Just x -> x
  Nothing -> error explanation

class Filterable a where
  type E a :: *
  filter :: (E a -> Bool) -> a -> a

instance Filterable [a] where
  type E [a] = a
  filter = P.filter

class LikeSet a where
  type K a :: *
  notElem :: K a -> a -> Bool
  elem    :: K a -> a -> Bool
  empty   :: a

instance LikeSet (I.IntMap v) where
  type K (I.IntMap v) = Int
  notElem = I.notMember
  elem = I.member
  empty = I.empty

instance Ord k => LikeSet (S.Set k) where
  type K (S.Set k) = k
  notElem = S.notMember
  elem = S.member
  empty = S.empty

instance Ord k => LikeSet (M.Map k v) where
  type K (M.Map k v) = k
  notElem = M.notMember
  elem = M.member
  empty = M.empty

-- XXX deprecated: partial
class Bang a where
  type I a :: *
  type V a :: *
  (!) :: a -> I a -> V a

instance Ord k => Bang (M.Map k v) where
  type I (M.Map k v) = k
  type V (M.Map k v) = v
  (!) = (M.!)

instance Bang (I.IntMap v) where
  type I (I.IntMap v) = Int
  type V (I.IntMap v) = v
  (!) = (I.!)

instance Bang (V.Vector a) where
  type I (V.Vector a) = Int
  type V (V.Vector a) = a
  (!) = (V.!)

instance SV.Storable a => Bang (SV.Vector a) where
  type I (SV.Vector a) = Int
  type V (SV.Vector a) = a
  (!) = (SV.!)

warn :: String -> a -> a
warn = warnC True

warnC :: Bool -> String -> a -> a
warnC c s x = if c then traceShow (decorated s) x else x
  where
  decorated str ="   >>> *** !!! " <> str <> " !!! *** <<<   "

asStateT :: Monad m => (a -> m a) -> StateT a m ()
asStateT = StateT . fmap2 ((),)

asState :: Monad m => (a -> a) -> StateT a m ()
asState = state . fmap ((),)

serializeWith :: (a -> Put) -> a -> ByteString
serializeWith f x = toStrict . runPut $ f x

parseWith :: Get a -> ByteString -> Either String a
parseWith get buf = case decoder' of
  Done _ _ v -> return v
  Fail _ _ str -> Left str
  Partial _ -> Left "Decoder is Partial"
  where
  decoder  = runGetIncremental get
  decoder' = pushEndOfInput (pushChunk decoder buf)

(§) :: (Integral a, Num b) => a -> b
(§) = P.fromIntegral

(<§>) :: (Integral a, Num b, Functor f) => f a -> f b
(<§>) = fmap (§)

foreign import ccall unsafe "string.h" memcpy
  :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)
foreign import ccall unsafe "string.h" memmove
  :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)
foreign import ccall unsafe "string.h" memset
  :: Ptr a -> CInt  -> CSize -> IO (Ptr a)

justLeft :: Either a b -> Maybe a
justLeft = \case
  Right _ -> Nothing
  Left x -> Just x

justRight :: Either a b -> Maybe b
justRight = \case
  Left _ -> Nothing
  Right x -> Just x

