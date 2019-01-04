#!/usr/bin/env stack
-- stack --resolver lts-6.0 runghc --package turtle
{-# LANGUAGE OverloadedStrings #-}
import Lib.Module
import qualified Lib.PrepareAssets as PrepareAssets

import Turtle
import qualified Data.Text as Text
import qualified Filesystem.Path.CurrentOS as Path

data Command
  = Rename Dot Dot
  | Remove Dot
  | Add Dot
  | PrepareAssets

subcommand1 long short d p = subcommand long  d p <|> subcommand short d p
moduleExample = "i.e. Some.Example.Module"
argModule name = Dot <$> argText name moduleExample

rename' r x y = if r then Rename y x else Rename x y
parser =
  subcommand1 "module" "m"
  "Module utilities." (
    subcommand1 "move" "mv"
    "Move (rename) a module" (
      rename'
        <$> switch "reverse" 'r' empty
        <*> argModule "from"
        <*> argModule "to"
    )
    <|>
    subcommand1 "remove" "rm"
    "Remove a module" (
      Remove <$> argModule "module"
    )
    <|>
    subcommand1 "create" "mk"
    "Create a module" (
      Add <$> argModule "module"
    )
  )
  <|>
  subcommand1 "prepare-assets" "p"
  "Prepare all assets." (
    pure PrepareAssets
  )

programDescription = "The Magnificent Haskell Heroes CLI Tool"
main = do
  command <- options programDescription parser
  case command of
    Rename x y -> rename x y
    Remove x   -> remove x
    Add x      -> add x
    PrepareAssets -> PrepareAssets.go

