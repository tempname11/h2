{-# LANGUAGE TemplateHaskell #-}
module Native.Stage.Loading (
  with,
  Loaded(..),
  Deps(..),
  WishIn(..),
  QueryOut(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.CreatureResource                           (CreatureResource)
import Heroes.SFXResource                                (SFXResource)
import Utils.NBChan                                      (NBChan)
import qualified Native.LoadingThread                      as LT
import qualified Stage.Links                               as L
import qualified Utils.NBChan                              as NBChan
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Either                                       (lefts)
import Data.Either                                       (rights)
import qualified Data.Map.Strict                           as M
import qualified Data.Set                                  as S
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  loadingChannels :: LT.LoadingChannels
}

data WishIn = WishIn {
  wishes :: L.Wishes
}

data QueryOut = QueryOut {
  loaded :: Loaded,
  isLoaded :: IsLoaded
}

data Loaded = Loaded {
  creatures :: Creature -> Maybe CreatureResource,
  sfxes :: SFX -> Maybe SFXResource
}

type IsLoaded = Either SFX Creature -> Bool

makeShorthands ''Loaded

--------------------------------------------------------------------------------

with :: Deps -> (IO QueryOut -> (WishIn -> IO ()) -> IO a) -> IO a
with (Deps {..}) next = do
  let (wishChan, loadedChan) = loadingChannels
  wref <- newIORef (empty, empty)
  qref <- newIORef (empty, empty)
  next (queryLoaded qref loadedChan) (wishLoaded wref wishChan)

--------------------------------------------------------------------------------

type WRef = IORef (WCData, WSData)
type QRef = IORef (QCData, QSData)
type QCData = Map Creature CreatureResource
type QSData = Map SFX SFXResource
type WCData = Set Creature
type WSData = Set SFX

--------------------------------------------------------------------------------

-- XXX
type X = Either (SFX, SFXResource) (Creature, CreatureResource)

queryLoaded :: QRef -> NBChan X -> IO QueryOut
queryLoaded ref loadedChan = do
  xs <- NBChan.drain loadedChan
  (cs, ss) <- readIORef ref
  let ss' = foldr (uncurry M.insert) ss (lefts xs)
  let cs' = foldr (uncurry M.insert) cs (rights xs)
  writeIORef ref (cs', ss')
  let creatures c = M.lookup c cs'
      sfxes s = M.lookup s ss'
      loaded = Loaded { creatures, sfxes }
      isLoaded (Left s) =
        case sfxes s of
          Just _  -> True
          Nothing -> False
      isLoaded (Right c) =
        case creatures c of
          Just _  -> True
          Nothing -> False
  return (QueryOut {..})

wishLoaded :: WRef -> NBChan (Either SFX Creature) -> WishIn -> IO ()
wishLoaded ref wishChan (WishIn {wishes}) = do
  (cs, ss) <- readIORef ref
  let isThere (Left s) = not (elem s ss)
      isThere (Right c) = not (elem c cs)
      notThere = filter isThere wishes
  NBChan.pour wishChan notThere
  let ss' = foldr S.insert ss (lefts notThere)
  let cs' = foldr S.insert cs (rights notThere)
  writeIORef ref (cs', ss')
