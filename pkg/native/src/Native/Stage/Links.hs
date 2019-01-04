module Native.Stage.Links where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import qualified Native.Resource
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified SDL
import qualified Data.Vector                               as V
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Renderer = SDL.Renderer
type DrawingAct = Native.DrawingAct
type Cursors = V.Vector (V.Vector SDL.Cursor)
type Loaded = Native.Resource.Loaded
type StaticResources = Native.Resource.StaticResources
