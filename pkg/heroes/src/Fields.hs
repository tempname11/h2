{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Fields where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common.TH                                         (declareShorthand)
import Control.Lens                                      (Lens)
import Data.Generics.Product                             (field)
import Data.Generics.Product                             (HasField)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

_animated :: HasField "animated" s t a b => Lens s t a b
_animated = field @"animated"

_subframeN :: HasField "subframeN" s t a b => Lens s t a b
_subframeN = field @"subframeN"

_actors :: HasField "actors" s t a b => Lens s t a b
_actors = field @"actors"

_curtain :: HasField "curtain" s t a b => Lens s t a b
_curtain = field @"curtain"

_props :: HasField "props" s t a b => Lens s t a b
_props = field @"props"

_position :: HasField "position" s t a b => Lens s t a b
_position = field @"position"

_height :: HasField "height" s t a b => Lens s t a b
_height = field @"height"

_facing :: HasField "facing" s t a b => Lens s t a b
_facing = field @"facing"

_sprite :: HasField "sprite" s t a b => Lens s t a b
_sprite = field @"sprite"

_frameN :: HasField "frameN" s t a b => Lens s t a b
_frameN = field @"frameN"

_groupN :: HasField "groupN" s t a b => Lens s t a b
_groupN = field @"groupN"

declareShorthand "abilities"
declareShorthand "actors"
declareShorthand "animated"
declareShorthand "animation"
declareShorthand "annotation"
declareShorthand "atlasTexture"
declareShorthand "attack"
declareShorthand "aux"
declareShorthand "background"
declareShorthand "base"
declareShorthand "baseHighlights"
declareShorthand "battle"
declareShorthand "bearing"
declareShorthand "bestMoves"
declareShorthand "bodies"
declareShorthand "box"
declareShorthand "cellOutline"
declareShorthand "cellShaded"
declareShorthand "cmds"
declareShorthand "conflict"
declareShorthand "copies"
declareShorthand "core"
declareShorthand "counter"
declareShorthand "creature"
declareShorthand "creatures"
declareShorthand "cursor"
declareShorthand "cursors"
declareShorthand "curtain"
declareShorthand "defence"
declareShorthand "diffs"
declareShorthand "dimensions"
declareShorthand "dst"
declareShorthand "eom"
declareShorthand "extension"
declareShorthand "external"
declareShorthand "facing"
declareShorthand "field"
declareShorthand "fighter"
declareShorthand "fighters"
declareShorthand "finalCache"
declareShorthand "flips"
declareShorthand "frame"
declareShorthand "frameCount"
declareShorthand "frameN"
declareShorthand "group"
declareShorthand "groupN"
declareShorthand "groups"
declareShorthand "height"
declareShorthand "hex"
declareShorthand "initialBattle"
declareShorthand "keyDown"
declareShorthand "keyPressed"
declareShorthand "keyUp"
declareShorthand "keys"
declareShorthand "meta"
declareShorthand "mouseButtons"
declareShorthand "mouseDown"
declareShorthand "mousePressed"
declareShorthand "mouseUp"
declareShorthand "mouseXY"
declareShorthand "movementHexes"
declareShorthand "moves"
declareShorthand "multiplacing"
declareShorthand "music"
declareShorthand "node"
declareShorthand "object"
declareShorthand "obstacles"
declareShorthand "offset"
declareShorthand "order"
declareShorthand "otype"
declareShorthand "outline"
declareShorthand "palette"
declareShorthand "paletteTexture"
declareShorthand "participants"
declareShorthand "peaceful"
declareShorthand "phase"
declareShorthand "place"
declareShorthand "placing"
declareShorthand "playerType"
declareShorthand "points"
declareShorthand "pokeCombinator"
declareShorthand "position"
declareShorthand "prediction"
declareShorthand "props"
declareShorthand "quitEvent"
declareShorthand "renderer"
declareShorthand "resources"
declareShorthand "score"
declareShorthand "screenBox"
declareShorthand "screenPlace"
declareShorthand "selectableFighters"
declareShorthand "selectionHexes"
declareShorthand "sfxes"
declareShorthand "shaded"
declareShorthand "sound"
declareShorthand "sounds"
declareShorthand "specials"
declareShorthand "speed"
declareShorthand "spell"
declareShorthand "sprite"
declareShorthand "src"
declareShorthand "stack"
declareShorthand "stampCache"
declareShorthand "stamps"
declareShorthand "state"
declareShorthand "subframeN"
declareShorthand "surface"
declareShorthand "team"
declareShorthand "texture"
declareShorthand "turn"
declareShorthand "value"
declareShorthand "walking"
declareShorthand "window"
declareShorthand "xid"
