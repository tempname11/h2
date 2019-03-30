{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Fields where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common.TH                                         (declareShorthand)
import Control.Lens                                      (Lens)
import Data.Generics.Product                             (field)
import Data.Generics.Product                             (HasField)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type FieldLens f = forall s t a b . HasField f s t a b => Lens s t a b

_animated :: FieldLens "animated"
_animated = field @"animated"

_subframeN :: FieldLens "subframeN"
_subframeN = field @"subframeN"

_actors :: FieldLens "actors"
_actors = field @"actors"

_curtain :: FieldLens "curtain"
_curtain = field @"curtain"

_props :: FieldLens "props"
_props = field @"props"

_moves :: FieldLens "moves"
_moves = field @"moves"

_peaceful :: FieldLens "peaceful"
_peaceful = field @"peaceful"

_position :: FieldLens "position"
_position = field @"position"

_height :: FieldLens "height"
_height = field @"height"

_facing :: FieldLens "facing"
_facing = field @"facing"

_sprite :: FieldLens "sprite"
_sprite = field @"sprite"

_frameN :: FieldLens "frameN"
_frameN = field @"frameN"

_groupN :: FieldLens "groupN"
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
