{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module Common.Misc where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Exception.Base                            (ErrorCall(ErrorCall))
import Control.Exception.Base                            (throwIO)
import Control.Lens                                      (Lens')
import Control.Monad.State                               (StateT(StateT))
import Control.Monad.State                               (state)
import Data.Binary.Get                                   (Decoder(..))
import Data.Binary.Get                                   (Get)
import Data.Binary.Get                                   (pushChunk)
import Data.Binary.Get                                   (pushEndOfInput)
import Data.Binary.Get                                   (runGetIncremental)
import Data.Binary.Put                                   (Put)
import Data.Binary.Put                                   (runPut)
import Data.ByteString                                   (ByteString)
import Data.ByteString.Lazy                              (toStrict)
import Foreign.Storable                                  (sizeOf)
import Foreign.Storable                                  (Storable)
import GHC.Exts                                          (Addr#)
import GHC.Exts                                          (Int(I#))
import GHC.Prim                                          (Int#)
import GHC.ForeignPtr                                    (ForeignPtr(..))
import Prelude                                             as P
import qualified Data.IntMap.Strict                        as I
import qualified Data.Map.Strict                           as M
import qualified Data.Set                                  as S
import qualified Data.Vector                               as V
import qualified Data.Vector.Storable                      as SV
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
#ifdef __GHCJS__
import Unsafe.Coerce                                     (unsafeCoerce)
#endif
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype Some a = Some a

data Buf = Buf Addr# Int#

unsafeToBuf :: forall a. Storable a => SV.Vector a -> Buf
unsafeToBuf v = Buf a l
  where
  !(ForeignPtr a _, n) = SV.unsafeToForeignPtr0 v
  !(I# l) = n * sizeOf @a undefined -- assuming tight packing: correct?

_some :: Lens' (Some a) a
_some f (Some a) = fmap Some (f a)

instance Show (Some a) where
  show _ = "Some"

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

class LikeSet a where
  type LikeKey a :: *
  notElem :: LikeKey a -> a -> Bool
  elem    :: LikeKey a -> a -> Bool
  empty   :: a

instance Eq v => LikeSet [v] where
  type LikeKey [v] = v
  notElem = P.notElem
  elem = P.elem
  empty = []

instance LikeSet (I.IntMap v) where
  type LikeKey (I.IntMap v) = Int
  notElem = I.notMember
  elem = I.member
  empty = I.empty

instance Ord k => LikeSet (S.Set k) where
  type LikeKey (S.Set k) = k
  notElem = S.notMember
  elem = S.member
  empty = S.empty

instance Ord k => LikeSet (M.Map k v) where
  type LikeKey (M.Map k v) = k
  notElem = M.notMember
  elem = M.member
  empty = M.empty

-- XXX deprecated: partial
class Bang a where
  type BangC a :: *
  type BangV a :: *
  (!) :: a -> BangC a -> BangV a

instance Ord k => Bang (M.Map k v) where
  type BangC (M.Map k v) = k
  type BangV (M.Map k v) = v
  (!) = (M.!)

instance Bang (I.IntMap v) where
  type BangC (I.IntMap v) = Int
  type BangV (I.IntMap v) = v
  (!) = (I.!)

instance Bang (V.Vector a) where
  type BangC (V.Vector a) = Int
  type BangV (V.Vector a) = a
  (!) = (V.!)

instance SV.Storable a => Bang (SV.Vector a) where
  type BangC (SV.Vector a) = Int
  type BangV (SV.Vector a) = a
  (!) = (SV.!)

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

justLeft :: Either a b -> Maybe a
justLeft = \case
  Right _ -> Nothing
  Left x -> Just x

justRight :: Either a b -> Maybe b
justRight = \case
  Left _ -> Nothing
  Right x -> Just x

raise :: String -> IO a
raise = throwIO . ErrorCall

#ifdef __GHCJS__
foreign import javascript unsafe
  "console.log($1)"
  consoleLog' :: Float -> IO () -- Float avoids JSVal import

consoleLog :: a -> IO ()
consoleLog = consoleLog' . unsafeCoerce
#endif
