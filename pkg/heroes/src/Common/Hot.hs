{-# LANGUAGE RankNTypes #-}
module Common.Hot (
  Hot(forget),
  Current(Current, this),
  currently,
  memo1
) where

import Prelude
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Mem.StableName as S
import qualified Data.HashTable.IO as H
import qualified System.Mem.Weak as W

newtype Hot a = Hot { forget :: a }
data Current a = Current { this :: a } -- deliberately not a newtype

currently :: Current a -> Hot (Current a)
currently = Hot

disabled :: Bool
disabled = False

-- XXX concurrency does not work. (try stm-containers?)
memo1 :: forall a b. (a -> b) -> (Hot a -> Hot b)
memo1 f =
  if disabled
  then Hot . f . (\(Hot x) -> x)
  else unsafePerformIO $ do
  (t :: H.BasicHashTable Int (W.Weak (b, S.StableName a))) <- H.new
  let miss x s h = do
        let y = f x
        let z =
              Just $ do -- it
                H.delete t h
        w <- W.mkWeak x (y, s) z
        H.insert t h w
        return y
  return $ \(Hot x) -> unsafePerformIO $ do
    s <- seq x $ S.makeStableName x
    let h = S.hashStableName s
    l <- H.lookup t h
    Hot <$> case l of
      Nothing -> miss x s h
      Just w -> do
        d <- W.deRefWeak w
        case d of
          Just (y, s') -> do
            if s == s'
            then do
              return y
            else miss x s h
          _ -> miss x s h
