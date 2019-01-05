{-# LANGUAGE Rank2Types #-}
module Native.Stage.IssueSoundCommands_ (
  with,
  Deps (..),
  In (..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes.UI.Sound                                   (Sound(..))
import Native
import Native.Platform ()
import Native.Stage.Loading                              (Loaded)
import qualified Heroes.UI.Sound                           as Sound
import qualified Stage.Links                               as L
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
import qualified SDL.Mixer                                 as Mix
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps { noDeps :: () }

data In = In {
  loaded :: Loaded,
  soundCommands :: L.SoundCommands
}

--------------------------------------------------------------------------------

with :: Deps -> ((In -> IO ()) -> IO a) -> IO a
with _ next = do
  ref <- newIORef empty
  next $ \in_ -> do
    d0 <- readIORef ref
    d1 <- sound in_ d0
    writeIORef ref d1

--------------------------------------------------------------------------------

type Data = Map Sound Mix.Channel

--------------------------------------------------------------------------------

sound :: In -> Data -> IO Data
sound (In {..}) =
  case soundCommands of
    Just commands -> execute commands
    Nothing -> stopAll
  where
  stopAll sound0 = mapM_ stop (M.elems sound0) >> return empty
  execute commands = execStateT $
    for_ commands $ \case
      Sound.PlayOnce s -> once s
      Sound.Start s -> do
        ch <- start s
        assign (at s) ch
      Sound.Stop s -> do
        m <- use $ at s
        case m of
          Nothing -> do
            lift $ print ("No sound playing for:" :: String, s)
            return ()
          Just channel -> do
            lift $ stop channel
            at s .= Nothing
  start = play Mix.Forever
  once = void . play Mix.Once
  stop = Mix.halt
  --
  play ::
    Mix.Times ->
    Sound ->
    StateT Data IO (Maybe Mix.Channel)
  play times s =
    case s of
      Sound'Creature c t -> case (loaded ^. creatures_) c of
        Just resource -> case M.lookup t $ resource ^. sounds_ of
          Just chunk -> do
            ch <- Mix.playOn (-1) times chunk
            return (Just ch)
          Nothing -> sorry
        Nothing -> sorry
      Sound'SFX sfx -> case (loaded ^. sfxes_) sfx of
        Just resource -> do
          ch <- Mix.playOn (-1) times (resource ^. sound_)
          return (Just ch)
        Nothing -> sorry
    where
    sorry = do
      lift $ print ("Missing sound for:" :: String, s)
      return Nothing

