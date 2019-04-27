module Heroes.Game where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Platform                                   (Platform)
import qualified Heroes.Requisites                         as RQ
import qualified Heroes.GFX                                as GFX
import qualified Heroes.SND                                as SND
import qualified Heroes.WND                                as WND
import qualified Heroes.AAI                                as AAI
import qualified Stage.Animation                           as A
import qualified Stage.Blackbox                            as B
import qualified Stage.DetermineInput                      as I
import qualified Stage.Loading                             as L
import qualified Stage.LoadingThread                       as LT
import qualified Stage.Prerequisites                       as PR
import qualified Stage.SystemLibraries                     as SL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

main' ::
  (
    GFX.GFX,
    WND.WND,
    SND.SND,
    I.DetermineInput,
    PR.Prerequisites,
    SL.SystemLibraries,
    Platform
  ) => IO ()
main' = do
  let noDeps = ()
  do
    putStrLn "--------------------"
    putStrLn "-- Starting up... --"
    putStrLn "--------------------"
  --
  id $
    SL.with $ \(SL.Prov {..}) ->
    WND.with $ \(WND.Prov {..}) ->
    GFX.with (GFX.Deps {..}) $ \(GFX.Prov {..}) ->
    SND.with (SND.Deps {..}) $ \(SND.Prov {..}) ->
    AAI.with (AAI.Deps {..}) $ \(AAI.Prov {..}) ->
    PR.with (PR.Deps {..}) $ \(PR.Prov {..})    ->
    RQ.with (RQ.Deps {..}) $ \(RQ.Prov {..})    ->
    LT.with (LT.Deps {..}) $ \(LT.Prov {..})    ->
    -------------------------------------------------
    L.with (L.Deps {..}) $ \queryLoaded wishLoaded ->
    I.with (I.Deps {..}) $ \determineInput         ->
    B.with (B.Deps {..}) $ \blackbox               ->
    A.with (A.Deps {..}) $ \animation              ->
    -------------------------------------------------
    fix $ \again -> do
      load
      L.QueryOut {..}  <- queryLoaded
      I.Out {..}       <- determineInput
      B.Out {exit, ..} <- blackbox (B.In {..})
      ----------------------------------------
      unless exit $ do
        -----------------------------------
        A.Out {..} <- animation (A.In {..})
        -----------------------------------
        wishLoaded   (L.WishIn {..})
        playSounds   (SND.In {..})
        changeCursor (WND.In {..})
        draw         (GFX.In {..})
        waitForVsync
        ------------
        again
  --
  do
    putStrLn "----------------"
    putStrLn "-- exiting... --"
    putStrLn "----------------"
