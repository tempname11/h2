module Heroes.AAI where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle                                            (Battle)
import Battle                                            (Move)
import Battle.AI.Search                                  (ai)
import Battle.Setup                                      (Setup)
import Common.Hot                                        (Current(..))
import Heroes
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified System.Mem.StableName                     as S
import qualified Control.Concurrent.Async                  as Async
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

newtype AIQuery = AIQuery (Current (Setup, Battle))
data AIResult
  = AIResult'OK [Move]
  | AIResult'Fail
  | AIResult'Pending

data Prov = Prov {
  queryAI :: IO (Maybe AIResult),
  askAI :: Maybe AIQuery -> IO ()
}

with :: Deps -> With Prov
with _ next = do
  ref <- newIORef Nothing
  let
    queryAI = do
      v <- readIORef ref
      case v of
        Nothing -> return Nothing
        Just (_, _, Just ms) -> return (Just (AIResult'OK ms))
        Just (s, a, Nothing) -> do
          m <- Async.poll a
          case m of
            Just (Right Nothing) -> do
              return (Just AIResult'Fail)
            Just (Right (Just ms)) -> do
              writeIORef ref (Just (s, a, Just ms))
              return (Just (AIResult'OK ms))
            Just (Left e) -> do
              print e
              return (Just AIResult'Fail)
            Nothing -> return (Just AIResult'Pending)
    --
    askAI = \case
      Nothing -> do
        v <- readIORef ref
        case v of
          Just (_, a, Nothing) -> do
            Async.cancel a
          _ -> return ()
      Just (AIQuery (Current c)) -> do
        s <- seq c $ S.makeStableName c
        v <- readIORef ref
        let
          spawn = do
            a <- Async.async $ do
              case ai c of
                Just ms -> return (Just ms)
                Nothing -> return Nothing
            writeIORef ref (Just (s, a, Nothing))
        --
        case v of
          Just (s', a', r) -> when (s /= s') $ do
            case r of
              Nothing -> do
                Async.cancel a'
              Just _ -> return ()
            spawn
          _ -> do
            spawn
  --
  next (Prov {..})
  -- XXX finalizer here?
