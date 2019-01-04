{-# LANGUAGE OverloadedStrings #-}
module Lib.Module where

import Turtle
import Lib
import Lib.Market'
import qualified Data.Text as Text
import qualified Filesystem.Path.CurrentOS as Path
import Control.Monad.Trans (lift)

classify (Dot d) = head $ flip match d $ (
    (contains "Web" *> pure "Web")
    <|>
    (contains "Native" *> pure "Native")
    <|>
    chars *> pure "Heroes"
 )

matches p t = case match p t of
  _:_ -> True
  _   -> False

market dot = do
  let Dot d = dot
  line <- consume
  produce line
  let cls = classify dot
      ptn = contains "exposed-modules" <> contains cls
  when (matches ptn line) $ do
    line2 <- consume
    case match (afterComma d) line2 of
      r:_ -> produce r
      _ -> return ()
    produce line2
  market dot

afterComma d = do
  cc <- selfless chars
  ", "
  _ <- chars
  pure $ cc <> ", " <> d

add dot = do
  -- add the .cabal reference
  replacing cabal $ \s -> do
    xs <- fold s $ Fold (flip (:)) [] reverse
    select $ trade (market dot) xs

  -- create the actual file
  do
    let Dot d = dot
    let file = slashFile (toSlash dot)
        content = "module " <> d <> " where\n"
    exists <- testfile file
    if exists
      then die "File exists!"
      else writeTextFile file content
    
remove dot = do
  -- remove the .cabal reference
  sh $ do
    let Dot t = dot
        pattern = spaces *> ", " *> text t
    infilter pattern cabal
  -- remove file
  sh $ do
    let file = slashFile (toSlash dot)
    good <- testfile file
    when good $ do
      rm file
  -- remove directory
  sh $ do
    let dir = slashDir (toSlash dot)
    good <- testdir dir
    when good $ do
      rmtree dir

{- all good! -}
rename dot1 dot2 = do
  let from = toSlash dot1
      to   = toSlash dot2
      dotted = (dot1, dot2)
  -- edit imports
  sh $ do
    file <- sources
    inplace (importR dotted <|> moduleR dotted) file
  -- edit cabal
  sh $ do
    inplace (simpleR dotted) cabal
  -- edit/rename the module file itself
  sh $ do
    let file = slashFile from
        dest = slashFile to
    good <- testfile file
    when good $ do
      mktree (Path.directory dest)
      mv file dest
  -- rename the corresponding directory if it exists
  sh $ do
    let dir  = slashDir from
        dest = slashDir to
    good <- testdir dir
    when good $ do
      mktree (Path.parent $ Path.directory dest)
      mv dir dest

replacing :: (MonadIO io) =>
  Path.FilePath ->
  (Shell Text -> Shell Text) ->
  io ()
replacing file x = liftIO $ runManaged $ do
  here <- pwd
  (tmpfile, handle) <- mktemp here "turtle2-replacing"
  outhandle handle (x $ input file)
  -- liftIO (hClose handle)
  copymod file tmpfile
  mv tmpfile file

infilter pattern file = replacing file $
  grep (invert pattern)

p <+> q = (<>) <$> p <*> q
newtype Dot = Dot Text
newtype Slash = Slash Text
toSlash (Dot x) = Slash $ Text.intercalate "/" $ Text.splitOn "." x
diff a b = Text.length a - Text.length b
padRight n t = t <> Text.replicate n " "
replace a b = text a *> pure b
replaceX a b = replaceX' a b <|> replace a b
replaceX' a b = do
  let sd = diff a b
  case sd of
    0 -> do
      replace a b
    d | d > 0 -> do
      text a
      cc <- selfless chars
      count 2 space
      pure $ (b <> cc <> Text.replicate (d + 2) " ")
    d' | otherwise -> do
      let d = -d'
      text a
      cc <- selfless chars
      space
      n <- length <$> bounded 0 d space
      let n' = max 1 (n - d)
      pure $ b <> cc <> Text.replicate n' " "

moduleR dd = begins "module" <> simpleR dd
importR dd = begins "import" <> simpleR dd
simpleR (Dot a, Dot b) = contains (replaceX a b)
sources = find (suffix ".hs") (Path.fromText src)
cabal = Path.fromText $ heroesPath <> "haskell-heroes.cabal"
slashFile (Slash x) = Path.fromText $ src <> x <> ".hs"
slashDir  (Slash x) = Path.fromText $ src <> x
src = heroesPath <> "src/"
