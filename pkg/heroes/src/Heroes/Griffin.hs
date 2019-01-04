module Heroes.Griffin where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.ByteString                           as B
import qualified Data.Vector                               as V
import qualified Data.Vector.Storable                      as SV
import qualified Data.Vector.Unboxed                       as UV
import qualified Data.IntMap.Strict                        as IntMap
import Data.Binary.Get                                   (Get)
import Data.Binary.Get                                   (skip)
import Data.Binary.Get                                   (isEmpty)
import Data.Binary.Get                                   (isolate)
import Data.Binary.Get                                   (getWord8)
import Data.Binary.Get                                   (bytesRead)
import Data.Binary.Get                                   (getWord32le)
import Data.Binary.Get                                   (getByteString)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

type Palette = SV.Vector (V4 Word8)

data Header = Header
  { hType        :: ! Word32
  , hWidth       :: ! Word32
  , hHeight      :: ! Word32
  , hGroupsCount :: ! Word32
  , hPalette     :: ! Palette
  }
  deriving (Show)

data Group = Group
  { sLength  :: ! Word32
  , sOffsets :: ! ( UV.Vector Int )
  }
  deriving (Show)

data Frame = Frame
  { fWidth   :: ! Word32
  , fHeight  :: ! Word32
  , fOffsetX :: ! Word32
  , fOffsetY :: ! Word32
  , fRows    :: ! ( V.Vector ByteString )
  }
  deriving (Show)

data Data = Data
  { dHeader :: ! Header
  , dGroups :: ! ( V.Vector Group )
  , dFrames :: ! ( IntMap Frame   )
  }
  deriving (Show)

--------------------------------------------------------------------------------

untilEmpty :: Get a -> Get [a]
untilEmpty g = isEmpty >>= \e -> if e
                                 then return []
                                 else (:) <$> g <*> untilEmpty g

getRGB :: Get (V4 Word8)
getRGB = V4 <$> g8 <*> g8 <*> g8 <*> return 255
  where g8 = getWord8

getPalette :: Get Palette
getPalette = SV.replicateM 256 getRGB

getHeader :: Get Header
getHeader = Header <$> g32 <*> g32 <*> g32 <*> g32 <*> getPalette
  where g32 = getWord32le

getData :: Get Data
getData = do
  h <- getHeader
  let count = (§) (hGroupsCount h)
  s <- V.replicateM count getGroup
  f <- IntMap.fromDistinctAscList <$> untilEmpty getKeyValue
  return $ Data h s f

  where getKeyValue = do
          offset <- (<§>) bytesRead
          frame <- getFrame
          return (offset, frame)

getFrame :: Get Frame
getFrame = do
  s <- getWord32le
  skip 12
  w <- getWord32le
  h <- getWord32le
  x <- getWord32le
  y <- getWord32le

  let dataSize = (§) s
  let numRows = (§) h
  r <- isolate dataSize $ do
    o <- UV.replicateM numRows getWord32le

    V.generateM numRows $ \i -> do
      at <- (<§>) bytesRead
      let end = if i == numRows - 1
                then (§) dataSize
                else (§) $ o UV.! (i + 1)
      let size = end - at

      (B.concat <$>) . isolate size $ untilEmpty $ do
        byte <- getWord8
        count <- (§) . (+1) <$> getWord8
        if byte == 0xFF
          then getByteString count
          else return $ B.replicate count byte

  return $ Frame w h x y r

skipName :: Get ()
skipName = skip 13

getGroup :: Get Group
getGroup = do
  skip 4
  l <- getWord32le
  skip 8
  let count = (§) l
  replicateM_ count skipName
  o <- UV.replicateM count ((<§>) getWord32le)
  return $ Group l o

parse :: ByteString -> Either String Data
parse = parseWith getData

