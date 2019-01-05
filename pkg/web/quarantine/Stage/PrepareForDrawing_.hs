{-# LANGUAGE TemplateHaskell #-}
module Web.Stage.PrepareForDrawing_ (
  with,
  Deps (..),
  In (..),
  Out (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import qualified Heroes.Cell                               as Cell
import qualified Stage.Links                               as L
import qualified Web.Drawing.Paletted                      as Paletted
import qualified Web.Drawing.Regular                       as Regular
import Battle                                            (FighterId)
import Animation.Scene                                   (Actor(..))
import Animation.Scene                                   (Handle(..))
import Heroes.UI                                         (fieldCenter)
import Web.ComplexSprite                                 (ComplexSprite(..))
import Web.Drawing                                       (CopySpec(..))
import Web.Drawing                                       (StaticSprite(..))
import Web.DrawingAct                                    (DrawingAct(..))
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  background :: L.Background,
  cellShaded :: L.CellShaded,
  cellOutline :: L.CellOutline
}

data In = In {
  loaded :: Loaded,
  scene :: L.Scene,
  lightHexes :: L.LightHexes,
  darkHexes :: L.DarkHexes
}

data Out = Out {
  wishes :: L.Wishes,
  drawingAct :: DrawingAct
}

--------------------------------------------------------------------------------

fullCopy :: StaticSprite -> Point V2 Float -> Regular.Cmd
fullCopy sprite screenPlace = Regular.Cmd sprite spec
  where
  spec = CopySpec {
    box = sprite ^. dimensions_,
    screenBox = sprite ^. dimensions_,
    place = 0,
    screenPlace
  }

-- @copypaste from Native.Stage.PrepareForDrawing_
onlyFighters :: (Handle, a) -> Maybe (Fighter, a)
onlyFighters (h, x) = case h of
  Handle'Fighter f -> Just (f, x)
  _ -> Nothing

onlyLoadedWith
  :: (FighterId -> Maybe ComplexSprite)
  -> (FighterId, Actor)
  -> Either Creature Paletted.Cmd
onlyLoadedWith spriteOf (fyr, actor) =
  case spriteOf fyr of
    Just sprite ->
      let frame = (sprite ^. meta_ . groups_) & (! g) & (! f) -- XXX partial...
          f = actor ^. frameN_
          g = actor ^. groupN_
          -- @copypaste from Native.Stage.PrepareForDrawing_.toCopy
          facing = actor ^. facing_
          screenPlace = (<§>) ((actor ^. position_) .+^ offset)
          offset = (frame ^. offset_) * sign
          sign = case facing of
            West -> V2 (-1) 1
            East -> 1
          spec = CopySpec {
            box   = (<§>) (frame ^. box_),
            place = (<§>) (frame ^. place_),
            screenPlace,
            screenBox = (<§>) ((frame ^. box_) * sign)
          }
          cmd = Paletted.Cmd sprite spec
      in Right cmd
    _ -> Left (Fighter.creature fyr)

with :: Deps -> ((In -> IO Out) -> IO a) -> IO a
with deps next = do
  next $ run deps

run :: Deps -> In -> IO Out
run (Deps {..}) (In {..}) = do
    -- @copypaste from Native.Stage.PrepareForDrawing_.extension
    let spriteOf f = loaded (Fighter.creature f) <&> view sprite_
        eitherList = map (onlyLoadedWith spriteOf) $
                      mapMaybe onlyFighters $
                        M.toList (scene ^. actors_)
        palettedCmds = mapMaybe justRight eitherList
        wishes = mapMaybe justLeft eitherList

    let drawingAct = DrawingAct {
          regularCmds = [bgCmd] <> shaded <> outline,
          palettedCmds,
          curtain = scene ^. curtain_
         }

    return $ Out {..}
  where
  bgCmd = fullCopy background 0
  outline = map (toCmd cellOutline) darkHexes
  shaded = map (toCmd cellShaded) lightHexes
  toCmd sprite hex = fullCopy sprite $ (<§>) (fieldCenter .+^ Cell.fromHex hex)
