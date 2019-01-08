{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE QuasiQuotes #-}
module Web.Artifacts.Prototype1 where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.UI                                         (viewportSize)
import Web
import Web.Platform ()
import Web.Audio                                         (Audio)
import qualified Heroes.Essentials                         as Essentials
import qualified Heroes.Input                              as Input
import qualified Heroes.FilePath                           as FilePath
import qualified Heroes.Platform                           as Platform
import qualified Web.Artifacts.Prototype1.Drawing          as Drawing
import qualified Web.KeyboardTrack                         as KeyboardTrack
import qualified Web.MouseTrack                            as MouseTrack
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.JSString                             as JSString
import qualified JavaScript.Web.AnimationFrame             as AF
import qualified JavaScript.Web.Canvas                     as Canvas
import qualified JavaScript.Web.XMLHttpRequest             as XHR
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

backDots :: String
backDots = "../"

bgPath :: String
bgPath = backDots <> Platform.productionPrefix <> "bg.png"

fortuneSoundPath :: String
fortuneSoundPath = backDots <> Platform.productionPrefix <> "Sounds/FORTUNE.wav"

pngPath :: String
pngPath = backDots <> Platform.productionPrefix <> "CELF.png"

-------------
-- Classes --
-------------

class Supports_new a where
  new :: IO a

class Supports_setSrc a where
  setSrc :: a -> JSString -> IO ()

class Supports_waitTillLoads a where
  waitTillLoads :: a -> IO ()

-----------
-- Audio --
-----------

foreign import javascript unsafe "new Audio()"
  newAudio :: IO Audio

foreign import javascript unsafe "$1.src = $2"
  audioSetSrc :: Audio -> JSString -> IO ()

foreign import javascript interruptible
  " \
  \ if ($1.readyState > 3) {    \
  \   setImmediate($c);         \
  \ } else {                    \
  \   $1.oncanplaythrough = $c; \
  \   $1.load();                \
  \ }                           \
  \ "
  audioWaitTillLoads :: Audio -> IO ()

foreign import javascript unsafe "$1.play().then(null, function(e) { console.log(e); });"
  audioPlay :: Audio -> IO ()

instance Supports_new Audio where
  new = newAudio

instance Supports_setSrc Audio where
  setSrc = audioSetSrc

instance Supports_waitTillLoads Audio where
  waitTillLoads = audioWaitTillLoads

-----------
-- Image --
-----------

foreign import javascript unsafe "new Image()"
  newImage :: IO Image

foreign import javascript unsafe "$1.src = $2"
  imageSetSrc :: Image -> JSString -> IO ()

foreign import javascript interruptible
  " \
  \ if ($1.complete) {  \
  \   setImmediate($c); \
  \ } else {            \
  \   $1.onload = $c    \
  \ }                   \
  \ "
  imageWaitTillLoads :: Image -> IO ()

--

instance Supports_new Image where
  new = newImage

instance Supports_setSrc Image where
  setSrc = imageSetSrc

instance Supports_waitTillLoads Image where
  waitTillLoads = imageWaitTillLoads

--------------------------------------------------------------------------------

foreign import javascript unsafe "document.body.appendChild($1)"
  appendCanvasToBody :: Canvas -> IO ()

loadEssentials :: IO Essentials.Essentials
loadEssentials = do
  let request = simpleXHR FilePath.essentialsBin1
  contents <- XHR.contents <$> XHR.xhrByteString request
  buf <- case contents of
    Just b -> return b
    Nothing -> raise "XHR contents are empty"
  case parseWith Essentials.getIt buf of
    Left str -> raise str
    Right e -> return e

main' :: IO ()
main' = do
  image <- new
  setSrc image (JSString.pack pngPath)
  waitTillLoads image
  --
  bg <- new
  setSrc bg (JSString.pack bgPath)
  waitTillLoads bg
  --
  sound <- new
  setSrc sound (JSString.pack fortuneSoundPath)
  waitTillLoads sound
  when False $ do -- XXX temporarily disabled
    audioPlay sound
  --
  canvas <- Canvas.create (viewportSize ^. _x) (viewportSize ^. _y)
  appendCanvasToBody canvas
  essentials <- loadEssentials
  static <- Drawing.init canvas image bg essentials
  print "All right, let's go!"
  --
  mtr <- MouseTrack.new canvas
  ktr <- KeyboardTrack.new
  --
  (place, mousePressed) <- MouseTrack.readLatest mtr
  keyPressed <- KeyboardTrack.readLatest ktr
  print (place, mousePressed Input.LMB, keyPressed Input.R)
  --
  fix $ \loop -> do
    timer <- AF.waitForAnimationFrame
    --
    Drawing.run timer static
    loop
