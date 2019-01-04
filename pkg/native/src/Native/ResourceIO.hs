{-# LANGUAGE TemplateHaskell #-}
module Native.ResourceIO where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.H3.Misc 
import Native 
import Native.Resource
import qualified Native.ComplexSprite                      as ComplexSprite
import qualified Heroes.UI.Cursor                          as Cursor
import qualified Heroes.FilePath                           as FilePath
import Platform.Config                                   (Config)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified SDL
import qualified Data.Map.Strict                           as M
import qualified Data.ByteString                           as B
import qualified Data.Vector                               as V
import qualified Data.Vector.Storable.Mutable              as MSV
import Data.List.Split (splitPlaces)
import SDL                                               (($=))
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

init :: Config => SDL.Renderer -> IO StaticResources
init renderer = do
  let loadSX = loadStatic renderer
      loadS = loadSX noOp noOp
      noOp = const $ return ()
      --
      obstacleS :: SDL.Surface -> IO ()
      obstacleS surface =
        SDL.surfaceColorKey surface $= Just (V4 0x00 0xFF 0xFF 0xFF)
      --
      cellS :: SDL.Surface -> IO ()
      cellS surface =
        SDL.surfaceColorKey surface $= Just (V4 0xFF 0x00 0xFF 0xFF)
      --
      cellT :: SDL.Texture -> IO ()
      cellT texture = do
        SDL.textureAlphaMod texture $= 0x80
        SDL.textureColorMod texture $= V3 0x00 0x00 0x00
  --
  background  <- loadS FilePath.background
  cellOutline <- loadSX cellS noOp FilePath.cellOutline
  cellShaded  <- loadSX cellS cellT FilePath.cellShaded
  --
  let allObstacles = [minBound .. maxBound]
      loadObstacle k = do
        v <- loadSX obstacleS noOp (FilePath.staticPathOf (oImgName k))
        return (k, v)
  --
  obstacleResourceMap <- M.fromList <$> for allObstacles loadObstacle
  --
  let obstacles t = obstacleResourceMap M.! t
  --
  return $ StaticResources {..}

loadStatic :: SDL.Renderer ->
          (SDL.Surface -> IO ()) ->
          (SDL.Texture -> IO ()) ->
          String -> IO StaticSprite
loadStatic renderer surfaceIO textureIO path = do
  traceShowM path
  surface <- SDL.loadBMP path
  void $ surfaceIO surface
  dimensions <- SDL.surfaceDimensions surface
  texture <- SDL.createTextureFromSurface renderer surface
  void $ textureIO texture
  SDL.freeSurface surface
  return $ StaticSprite {
    texture = texture,
    dimensions = dimensions
  }

destroyStatic :: StaticSprite -> IO ()
destroyStatic s = SDL.destroyTexture (s ^. texture_)

fini ::
  StaticResources ->
  IO ()
fini static = do
  destroyStatic (static ^. background_)
  destroyStatic (static ^. cellShaded_)
  destroyStatic (static ^. cellOutline_)

--------------------------------------------------------------------------------

-- Leftover bits for handling animated sprites, i.e. spells.
-- XXX remove?
{-
data AnimatedSprite = AnimatedSprite {
  as'texture :: SDL.Texture,
  as'groups  :: V.Vector Atlas.Group
}

loadAnimated :: SDL.Renderer -> String -> IO AnimatedSprite
loadAnimated renderer path = do
  c <- loadComplex path
  let surface = c ^. surface_
      groups  = c ^. groups_
  texture <- SDL.createTextureFromSurface renderer surface
  destroyComplex c
  return $ AnimatedSprite {
    as'texture = texture,
    as'groups  = groups
  }

destroyAnimated :: AnimatedSprite -> IO ()
destroyAnimated s = SDL.destroyTexture (s ^. texture_)

makeShorthands ''AnimatedSprite
-}

--------------------------------------------------------------------------------

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
        (surface, _) <- ComplexSprite.createPalettedSurface mpixels colors (V2 w h)
        cursor11 <- SDL.createColorCursor surface info
        SDL.freeSurface surface -- not sure if we can free the surface.
                                -- seems to work.
        return cursor11

  let places = map length infos
      decat = splitPlaces places

  fmap V.fromList . V.fromList . decat <$>
    zipWithM make (V.toList allocations) (concat infos)

