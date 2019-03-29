{-# OPTIONS_GHC -Wno-orphans #-}
module Native.WND'SDL () where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.Aux                                        (Annotation(..))
import Heroes.WND
import Heroes.UI                                         (viewportSize)
import Native
import Native.Platform ()
import Native.Utils                                      (createPalettedSurface)
import qualified Heroes.Bearing                            as Bearing
import qualified Heroes.UI.Cursor                          as Cursor
import qualified Native.UI.Cursor                          as Cursor
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.List.Split                                   (splitPlaces)
import Data.String                                       (fromString)
import SDL                                               (($=))
import qualified Data.ByteString                           as B
import qualified Data.Vector                               as V
import qualified Data.Vector.Storable.Mutable              as MSV
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Data = Maybe IData

data IData = IData {
  _frame   :: Int,
  _group   :: Int,
  _counter :: Int
}

instance WND where
  type Window = SDL.Window
  type CursorResources = V.Vector (V.Vector SDL.Cursor)
  with next = do
    SDL.initialize [
        SDL.InitAudio,
        SDL.InitVideo, 
        SDL.InitEvents
      ]
    window <- SDL.createWindow (fromString "Fight!") windowConfig
    cursorResources <- V.concat <$> for Cursor.stuff (uncurry loadCursor)
    ref <- newIORef Nothing
    --
    let
      changeCursor in_ = do
        d0 <- readIORef ref
        d1 <- changeCursor' in_ d0
        writeIORef ref d1
      --
      waitForVsync = SDL.glSwapWindow window -- XXX
    --
    next $ Prov {..}
    (mapM_ . mapM_) SDL.freeCursor cursorResources
    SDL.destroyWindow window
    SDL.quit

windowConfig :: SDL.WindowConfig
windowConfig =
  SDL.defaultWindow
  {
    SDL.windowPosition = SDL.Absolute (P $ V2 100 100),
    SDL.windowInitialSize = (<§>) viewportSize
  }

changeCursor' :: Platform => In -> Data -> IO Data
changeCursor' (In {..}) d = do
  SDL.activeCursor $= cursorResources ! g' ! f'
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

loadCursor :: [[Point V2 CInt]]
           -> String
           -> IO (V.Vector (V.Vector SDL.Cursor))
loadCursor infos path = do
  putStrLn $ "Loading... " <> path
  buf <- B.readFile path

  blueprint <- case Cursor.parse buf of
                 Left str -> raise str
                 Right b -> return b

  let (V2 w h) = Cursor.bDimensions blueprint
      colors     = Cursor.bPalette blueprint
      pokeCombinator = Cursor.bPokeCombinator blueprint
      count = Cursor.bCount blueprint
      size = (§) (w * h)

  let iCount = sum $ map length infos
  when (count /= iCount) $ do
    print (path, count, iCount)
    raise "Cursor info & .def have unequal length"

  allocations <- V.replicateM count $ MSV.replicate size 0

  let poke i (V2 x y) bytes = generateM_ (B.length bytes) go
        where
        offset = (§) (x + y * w)
        go :: Int -> IO ()
        go j = MSV.write mpixels (offset + j) byte
          where byte = B.index bytes j
                mpixels = allocations ! i

  pokeCombinator poke

  -- allocations and infos should have equal length here
  let make mpixels info = do
        (surface, _) <- createPalettedSurface mpixels colors (V2 w h)
        cursor11 <- SDL.createColorCursor surface info
        SDL.freeSurface surface -- not sure if we can free the surface.
                                -- seems to work.
        return cursor11

  let places = map length infos
      decat = splitPlaces places

  fmap V.fromList . V.fromList . decat <$>
    zipWithM make (V.toList allocations) (concat infos)
