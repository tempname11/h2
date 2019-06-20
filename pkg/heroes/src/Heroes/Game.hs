module Heroes.Game where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Platform                                   (Platform)
import qualified Heroes.AAI                                as AAI
import qualified Heroes.GFX                                as GFX
import qualified Heroes.Requisites                         as RQ
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
    RQ.with (RQ.Deps {..}) $ \(RQ.Prov {..}) ->
    LT.with (LT.Deps {..}) $ \(LT.Prov {..}) ->
    L.with (L.Deps {..}) $ \queryLoaded wishLoaded ->
    I.with (I.Deps {..}) $ \determineInput ->
    Root.with (Root.Deps {..}) $ \rootProv ->
    do
      (fullInput'E, fullInput'T) <- J.newEvent
      (loaded'E, loaded'T) <- J.newEvent
      exit'B <- J.run $ do
        loaded'B <- J.hold L.emptyLoaded loaded'E
        (out'E, exit'B) <- (rootProv ^. #new) fullInput'E loaded'B
        _ <- J.subscribe () out'E $ \(Root.Out {..}) -> liftIO $ do
            wishLoaded (L.WishIn {..})
            playSounds (SND.In {..})
            changeCursor (WND.In {..})
            draw drawCallback
        return exit'B
        --
      fix $ \again -> do
        load
        L.QueryOut {..} <- queryLoaded
        J.fire [loaded'T J.==> loaded]
        I.Out {..} <- determineInput
        J.fire [fullInput'T J.==> fullInput]
        exit <- J.run $ J.sample exit'B
        waitForVsync
        when (not exit) again
