module Heroes.Game where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Platform                                   (Platform)
import qualified Heroes.AAI                                as AAI
import qualified Heroes.GFX                                as GFX
import qualified Heroes.Root                               as Root
import qualified Heroes.SND                                as SND
import qualified Heroes.WND                                as WND
import qualified Stage.DetermineInput                      as I
import qualified Stage.Loading                             as L
import qualified Stage.LoadingThread                       as LT
import qualified Stage.Prerequisites                       as PR
import qualified Stage.SystemLibraries                     as SL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Reflex.Jumpstart                          as J
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

main' ::
  (
    WSC,
    GFX,
    WND,
    SND,
    I.DetermineInput,
    PR.Prerequisites,
    SL.SystemLibraries,
    Platform
  ) => IO ()
main' = do
  let noDeps = ()
  --
  id $
    SL.with $ \(SL.Prov {..}) ->
    WND.with $ \(WND.Prov {..}) ->
    PR.with (PR.Deps {..}) $ \(PR.Prov {..}) ->
    GFX.with (GFX.Deps {..}) $ \(GFX.Prov {..}) ->
    SND.with (SND.Deps {..}) $ \(SND.Prov {..}) ->
    AAI.with (AAI.Deps {..}) $ \(AAI.Prov {..}) ->
    LT.with (LT.Deps {..}) $ \(LT.Prov {..}) ->
    L.with (L.Deps {..}) $ \queryLoaded wishLoaded ->
    I.with (I.Deps {..}) $ \determineInput ->
    do
      (fullInput'E, fullInput'T) <- J.extern
      (loaded'E, loaded'T) <- J.extern
      loaded'B <- J.hold L.emptyLoaded loaded'E
      (out'E, exit'B) <- Root.new (Root.Deps {..}) fullInput'E loaded'B
      let
        io'E = out'E <&> \(Root.Out {..}) -> do
          wishLoaded (L.WishIn {..})
          playSounds (SND.In {..})
          changeCursor (WND.In {..})
          draw drawCallback
        --
      io'B <- J.hold (return ()) io'E
      fix $ \again -> do
        load
        L.QueryOut {..} <- queryLoaded
        J.fire [loaded'T loaded]
        I.Out {..} <- determineInput
        J.fire [fullInput'T fullInput]
        join $ J.sample io'B
        exit <- J.sample exit'B
        waitForVsync
        when (not exit) again
