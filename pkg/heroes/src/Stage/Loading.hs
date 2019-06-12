{-# LANGUAGE FlexibleContexts #-}
module Stage.Loading (
  with,
  emptyLoaded,
  Loaded(..),
  Deps(..),
  WishIn(..),
  QueryOut(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.CreatureResource                           (CreatureResource)
import Heroes.SFXResource                                (SFXResource)
import Stage.LoadingThread                               (LoadRequest(..))
import Stage.LoadingThread                               (LoadResult(..))
import Utils.NBChan                                      (NBChan)
import qualified Stage.LoadingThread                       as LT
import qualified Utils.NBChan                              as NBChan
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
import qualified Data.Set                                  as S
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  loadingChannels :: LT.LoadingChannels
}

data WishIn = WishIn {
  loadRequests :: Set LoadRequest
}

data QueryOut = QueryOut {
  loaded :: Loaded
}

data Loaded = Loaded {
  creatures :: Creature -> Maybe CreatureResource,
  sfxes :: SFX -> Maybe SFXResource
} deriving (Generic)

type Maps =
  (
    Map Creature CreatureResource,
    Map SFX SFXResource
  )

--------------------------------------------------------------------------------

with :: Deps -> (IO QueryOut -> (WishIn -> IO ()) -> IO a) -> IO a
with (Deps {..}) next = do
  let (wishChan, loadedChan) = loadingChannels
  requestsRef <- newIORef empty
  qref <- newIORef (empty, empty)
  next (queryLoaded qref loadedChan) (wishLoaded requestsRef wishChan)

emptyLoaded :: Loaded
emptyLoaded = Loaded { creatures = const Nothing, sfxes = const Nothing }

queryLoaded :: IORef Maps -> NBChan LoadResult -> IO QueryOut
queryLoaded ref loadedChan = do
  rs <- NBChan.take loadedChan
  maps <- readIORef ref
  --
  let
    maps' = foldr step maps rs
    step (LoadResult'Creature k v) = over _1 $ M.insert k v
    step (LoadResult'SFX k v) = over _2 $ M.insert k v
    loaded = Loaded {
      creatures = \c -> M.lookup c (view _1 maps'),
      sfxes = \s -> M.lookup s (view _2 maps')
    }
  --
  writeIORef ref maps'
  return (QueryOut {..})

wishLoaded :: IORef (Set LoadRequest) -> NBChan LoadRequest -> WishIn -> IO ()
wishLoaded ref wishChan (WishIn {..}) = do
  prevRequests <- readIORef ref
  let notThere = S.filter (not . flip elem prevRequests) loadRequests
  NBChan.put wishChan (S.toList notThere)
  writeIORef ref (S.union loadRequests prevRequests)
