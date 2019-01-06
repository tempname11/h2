module Heroes.Game where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Platform                                   (Platform)
import qualified Heroes.Requisites                         as RQ
import qualified Stage.Animation                           as A
import qualified Stage.Blackbox                            as B
import qualified Stage.ChangeCursor                        as C
import qualified Stage.ControlSound                        as S
import qualified Stage.DetermineInput                      as I
import qualified Stage.Draw                                as D
import qualified Stage.Loading                             as L
import qualified Stage.LoadingThread                       as LT
import qualified Stage.Prerequisites                       as PR
import qualified Stage.SystemLibraries                     as SL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

main' ::
  (
    C.ChangeCursor,
    D.Draw,
    I.DetermineInput,
    PR.Prerequisites,
    S.ControlSound,
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
  SL.with $ \(SL.Prov {..}) ->
    PR.with (PR.Deps {..}) $ \(PR.Prov {..}) ->
    RQ.with (RQ.Deps {..}) $ \(RQ.Prov {..}) ->
    LT.with (LT.Deps {..}) $ \(LT.Prov {..}) ->
    -------------------------------------------------
    L.with (L.Deps {..}) $ \queryLoaded wishLoaded ->
    I.with (I.Deps {..}) $ \determineInput         ->
    B.with (B.Deps {..}) $ \blackbox               ->
    A.with (A.Deps {..}) $ \animation              ->
    S.with (S.Deps {..}) $ \issueSoundCommands_    ->
    C.with (C.Deps {..}) $ \changeCursor           ->
    D.with (D.Deps {..}) $ \draw                   ->
    --------------------------------------------------
    fix $ \again -> do
        ----------------------------------------
        L.QueryOut {..}  <- queryLoaded
        I.Out {..}       <- determineInput
        B.Out {exit, ..} <- blackbox (B.In {..})
        ----------------------------------------
        unless exit $ do
          --------------------------------------------
          A.Out {..} <- animation          (A.In {..})
          --------------------------------------------
          wishLoaded          (L.WishIn {..})
          issueSoundCommands_ (S.In {..})
          changeCursor        (C.In {..})
          draw                (D.In {..})
          --------------------------------
          again
  --
  do
    putStrLn "----------------"
    putStrLn "-- exiting... --"
    putStrLn "----------------"
