{-# LANGUAGE TemplateHaskell #-}
module Web.ComplexSprite where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.SpriteMeta                                 (Meta)
import Web
import Web.Drawing.Utilities                             (makePaletteTexture)
import Web.Drawing.Utilities                             (makeTexture)
import qualified Heroes.H3                                 as H3
import qualified Native.FilePath                           as FilePath
import qualified Web.GLES                                  as GL
import qualified Web.Image                                 as Image
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified JavaScript.TypedArray                     as TypedArray
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

load :: GL.Context -> Creature -> Meta -> IO ComplexSprite
load ctx c meta = do
  image <- Image.load (pngPathOf c)
  atlasTexture <- makeTexture ctx image
  let palette = meta ^. palette_
  paletteArray <- TypedArray.create 1024
  for_ [0..255] $ \i -> do
    let V4 r g b a = if i > 8
                     then (<§>) (palette ! i)
                     else 0
    TypedArray.unsafeSetIndex (i * 4 + 0) r paletteArray
    TypedArray.unsafeSetIndex (i * 4 + 1) g paletteArray
    TypedArray.unsafeSetIndex (i * 4 + 2) b paletteArray
    TypedArray.unsafeSetIndex (i * 4 + 3) a paletteArray
  paletteTexture <- makePaletteTexture ctx paletteArray
  return $ ComplexSprite { .. }

destroy :: ()
destroy = ()
-- XXX this is probably needed.
