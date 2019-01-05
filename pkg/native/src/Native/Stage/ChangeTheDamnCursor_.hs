module Native.Stage.ChangeTheDamnCursor_ (
  with,
  Deps (..),
  In (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Aux
import Native
import Native.Prerequisites                              (Cursors)
import qualified Heroes.Bearing                            as Bearing
import qualified Heroes.UI.Cursor                          as Cursor
import qualified Native.UI.Cursor                          as Cursor
import qualified Stage.Links                               as L
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import SDL                                               (($=))
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data In = In {
  intent :: L.Intent,
  cursors :: Cursors
}

--------------------------------------------------------------------------------

with :: Platform => Deps -> ((In -> IO ()) -> IO a) -> IO a
with _ next = do
  ref <- newIORef Nothing
  next $ \in_ -> do
    d0 <- readIORef ref
    d1 <- cursor in_ d0
    writeIORef ref d1

--------------------------------------------------------------------------------

type Data = Maybe IData

data IData = IData {
  _frame   :: Int,
  _group   :: Int,
  _counter :: Int
}

--------------------------------------------------------------------------------

cursor :: Platform => In -> Data -> IO Data
cursor (In {..}) d = do
  SDL.activeCursor $= cursors ! g' ! f'
  return d'
  where
  --
  d' = Just (IData f' g' c')
  g' = Cursor.number $ case intent of
    Just Running -> Cursor.Run
    Just Pondering -> Cursor.Question
    Just (MeleeAttackingFrom b) -> Cursor.Sword (Cursor.To $ Bearing.opposite b)
    Just RangeAttacking -> Cursor.Arrow
    Just Selecting -> Cursor.Normal
    Nothing -> Cursor.Normal
  --
  (c', f') = case d of
    Nothing -> (0, 0)
    Just (IData f g c)
      | g /= g'   -> (0, 0)
      | otherwise ->
          let _15fps = c1 `mod` 4 == 0
              c1 = c + 1
              f1 = if | _15fps -> (f + 1) `mod` Cursor.lengths ! g
                      | otherwise -> f
          in (c1, f1)



