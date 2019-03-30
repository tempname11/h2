#!/usr/bin/env stack
-- stack --resolver lts-9.21 runghc --package turtle
{-# LANGUAGE OverloadedStrings #-}
import qualified Lib.PrepareAssets as PrepareAssets

import Turtle
import qualified Data.Text as Text
import qualified Filesystem.Path.CurrentOS as Path

data Command = PrepareAssets

subcommand1 long short d p = subcommand long  d p <|> subcommand short d p

parser =
  subcommand1 "prepare-assets" "p"
  "Prepare all assets." (
    pure PrepareAssets
  )

programDescription = "The Magnificent Haskell Heroes CLI Tool"
main = do
  command <- options programDescription parser
  case command of
    PrepareAssets -> PrepareAssets.go

