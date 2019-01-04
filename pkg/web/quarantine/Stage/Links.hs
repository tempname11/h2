module Web.Stage.Links where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Web
import qualified Web.DrawingAct                            as DrawingAct
import qualified Web.Drawing                               as Drawing
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Web.GLES                                  as GL
import qualified Web.Drawing.Quad                          as Quad
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type TheCanvas = Canvas
type TheContext = GL.Context
type DrawingAct = DrawingAct.DrawingAct
type QBuffer = Quad.QBuffer
type StaticResources = Web.Resource.StaticResources
