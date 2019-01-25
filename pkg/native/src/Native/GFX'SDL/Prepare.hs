{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Native.GFX'SDL.Prepare (
  with,
  Deps (..),
  In (..),
  Out (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Animation.Scene                                   (Actor)
import Animation.Scene                                   (Handle(..))
import Animation.Scene                                   (Prop(..))
import Animation.Scene                                   (Scene)
import Battle                                            (FighterId)
import Heroes.Atlas                                      (Frame)
import Heroes.Scaling
import Heroes.UI
import Heroes.UI.Specials                                (Specials)
import Native
import Native.GFX'SDL.Common
import qualified Heroes.GFX                                as GFX
import qualified Heroes.UI.Specials                        as Specials
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Monad.Morph                               (generalize)
import Control.Monad.Morph                               (hoist)
import Prelude                                           (truncate)
import SDL                                               (($=))
import qualified Data.Map.Strict                           as M
import qualified SDL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  renderer :: SDL.Renderer,
  staticResources :: GFX.StaticResources
}

data In = In {
  darkHexes :: [Hex],
  extraColor :: FighterId -> Maybe Color,
  lightHexes :: [Hex],
  scene :: Scene
}

data Out = Out {
  drawingAct :: DrawingAct
}

--------------------------------------------------------------------------------

with :: Deps -> ((In -> IO Out) -> IO a) -> IO a
with deps next = do
  ref <- newIORef initially
  result <- next (run deps ref)
  d1 <- readIORef ref
  for_ (d1 ^. stampCache_) destroyStamp
  return result

--------------------------------------------------------------------------------

data Data = Data {
  stampCache :: Map Handle Stamp,
  finalCache :: Map FighterId Specials
}

type Ref = IORef Data
type HA = (Handle, Actor)
type HFA = (Handle, Frame, Actor)

--------------------------------------------------------------------------------

destroyStamp :: Stamp -> IO ()
destroyStamp s = do
  SDL.freeSurface    (s ^. surface_)
  SDL.destroyTexture (s ^. texture_)

initially :: Data
initially = Data {
  stampCache = empty,
  finalCache = empty
}

run :: Deps -> Ref -> In -> IO Out
run deps ref in_ = do
  d0 <- readIORef ref
  (out, d1) <- flip runStateT d0 $ run' deps in_
  writeIORef ref d1
  return out

toHFA :: HA -> HFA
toHFA (h, actor) =
  let frame = (actor ^. _sprite . groups_) & (! g) & (! f) -- XXX partial...
      f = actor ^. _frameN
      g = actor ^. _groupN
  in (h, frame, actor)

run' :: Deps -> In -> StateT Data IO Out
run' (Deps {..}) (In {..}) = do
  let finalSpecialsOf = determineFinalSpecials extraColor
      allActors = M.toList (scene ^. _actors)
      hfas = toHFA <$> allActors
  --
  palettesToUpdate <- morf $ zoom finalCache_ $
    diffSpecials finalSpecialsOf hfas
  --
  stampsToDraw <- zoom stampCache_ $
    equipStamps renderer hfas
  --
  void $ lift $
    updatePalettes palettesToUpdate
  --
  void $ lift $
    updateStamps stampsToDraw
  --
  let
    handleCopies = fmap toHandleCopy . sortBy comparingY $ stampsToDraw
    obstacleCopies =
      fmap toPropCopy $
      fmap (over _1 ((staticResources ^. obstacles_) . view otype_)) $
      M.assocs $
      (scene ^. _props)
    drawingAct = DrawingAct {
      curtain = truncate (255 * scene ^. _curtain),
      outline = lightHexes,
      shaded = darkHexes,
      copies = obstacleCopies <> handleCopies
    }
  return (Out {..})
  where
  comparingY = (comparing . view) (_1 . _3 . _position . _y)
  morf = hoist generalize

determineFinalSpecials :: (FighterId -> Maybe Color) -> FighterId -> Specials
determineFinalSpecials extraColor fyr =
  let result = Specials.fromOutlineColor <$> extraColor fyr
  in maybe Specials.defaults id result

diffSpecials
  :: (FighterId -> Specials)
  -> [HFA]
  -> State (Map FighterId Specials) [(SDL.Palette, Specials)]
diffSpecials finalSpecialsOf xs
  = catMaybes <$> for xs function
  where
  nevermind = return Nothing
  function (Handle'SFX _, _, _) = nevermind
  function (Handle'Fighter fyr, _, actor) =
    do
      let sprite = actor ^. _sprite
      let new = Just (finalSpecialsOf fyr)
      old <- use $ at fyr
      if (old /= new)
      then do
        at fyr .= new
        let palette = sprite ^. palette_
        return $ (palette,) <$> new
      else
        nevermind

updatePalettes :: [(SDL.Palette, Specials)] -> IO ()
updatePalettes = mapM_ $ \(palette, specials) ->
  SDL.setPaletteColors palette specials 0

equipStamps
  :: SDL.Renderer
  -> [HFA]
  -> StateT (Map Handle Stamp) IO [(HFA, Stamp)]
equipStamps renderer xs
  = for xs function
  where
  function x@(fyr, _, _) = do
    cache <- get
    stamp <- case M.lookup fyr cache of
      Nothing -> do
        surface <- SDL.createRGBSurface stampSize SDL.RGBA8888
        texture <- SDL.createTexture renderer
          SDL.RGBA8888 SDL.TextureAccessStreaming stampSize
        SDL.textureBlendMode texture $= SDL.BlendAlphaBlend
        let s = Stamp {
          surface = surface,
          texture = texture
        }
        put $ M.insert fyr s cache
        return s
      Just s ->
        return s
    return (x, stamp)

updateStamps :: [(HFA, Stamp)] -> IO ()
updateStamps xs
  = for_ xs function
  where
  function :: (HFA, Stamp) -> IO ()
  function ((_, frame, actor), stamp) = 
    do
      let sprite = actor ^. _sprite
      let spriteSurface = sprite ^. surface_
          stampSurface  = stamp  ^. surface_
          stampTexture  = stamp  ^. texture_
          spriteRect = Just $ SDL.Rectangle p box
          stampDest = Just 0
          box = frame ^. box_
          p   = frame ^. place_
      void $ SDL.surfaceBlit spriteSurface spriteRect stampSurface stampDest
      copySurfaceToTexture stampSurface stampTexture stampByteSize

copySurfaceToTexture :: SDL.Surface -> SDL.Texture -> CInt -> IO ()
copySurfaceToTexture s t len = do
  srcPtr      <- SDL.lockSurface s >> SDL.surfacePixels s
  (dstPtr, _) <- SDL.lockTexture t Nothing
  void $ memcpy dstPtr srcPtr ((§) len)
  SDL.unlockTexture t
  SDL.unlockSurface s

toPropCopy :: (StaticSprite'SDL, Prop) -> CopyCommand
toPropCopy (StaticSprite'SDL {..}, Prop {..}) = CopyCommand {
    texture,
    src = Just $ SDL.Rectangle 0 dimensions,
    dst = Just $ SDL.Rectangle (rescaled position) (rescaled dimensions),
    flips =
      case facing of
        West -> V2 True  False
        East -> V2 False False
  }

toHandleCopy
  :: (HFA, Stamp)
  -> CopyCommand
toHandleCopy ((_, frame, actor), stamp) = CopyCommand {
    texture = stamp ^. texture_,
    src = Just $ SDL.Rectangle 0 box,
    dst = Just $ SDL.Rectangle (rescaled screenPosition) (rescaled box),
    flips =
      case facing of
        West -> V2 True  False
        East -> V2 False False
  }
  where
  facing = actor ^. _facing
  screenPosition = (actor ^. _position) .+^ offset .-^ (V2 0 (actor ^. _height))
  offset = (frame ^. offset_) * sign + flipOffset
  flipOffset =
    case facing of
      West -> box * V2 (-1) 0
      East -> 0
  sign =
    case facing of
      West -> V2 (-1) 1
      East -> 1
  box = frame ^. box_

stampSize :: V2 CInt
stampSize = V2 256 256

stampByteSize :: CInt
stampByteSize = (stampSize ^. _x) * (stampSize ^. _y) * 4

makeShorthands ''Data
