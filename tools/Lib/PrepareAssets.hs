{-# LANGUAGE OverloadedStrings #-}
module Lib.PrepareAssets (go) where

import Turtle
import qualified Data.Text as Text
import qualified Filesystem.Path.CurrentOS as Path
import Control.Monad.Trans (lift)

magik from to mods = do
  -- Native uses bmp, Web uses png for now, do both.
  cp' from (Text.replace ".png" ".bmp" to)
  procs "convert" ([from] ++ mods ++ ["png32:"<>to]) empty
  printf w $ [from] ++ mods ++ ["png32:"<>to]

cp' from to = cp (Path.fromText from) (Path.fromText to)

tm = Text.words "-transparent magenta"
tc = Text.words "-transparent cyan"
op = Text.words "-channel A -evaluate set 1"
a8 = Text.words "-channel A -evaluate multiply 0.8"
bk = Text.words "-channel RGB -evaluate set 0"

h3 :: Text
h3 = "../h3-assets/"

pcx :: Text
pcx = "../h3-assets/PCX/"

battleBgs :: Text
battleBgs = "../h3-assets/categorized/battle-bgs/"

obstacles :: Text
obstacles = "../h3-assets/categorized/obstacles/"

prod :: Text
prod = "../.production-assets/"

go :: (MonadIO io) => io ()
go = do
  magik (pcx <> "CCellShd.bmp") (prod <> "cell-shaded.png" ) $ tm<>bk<>a8
  magik (pcx <> "CCellGrd.bmp") (prod <> "cell-outline.png") $ tm
  magik (battleBgs <> "CmBkDes.bmp") (prod <> "bg.png") $ op
  magik (obstacles <> "ObCFL00.bmp") (prod <> "ObCFL00.png") $ tc
  cp' (h3 <> "Silent.wav") (prod <> "silent.wav")
