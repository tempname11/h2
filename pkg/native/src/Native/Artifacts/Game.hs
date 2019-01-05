-- XXX marked for deletion in favor of Heroes/Game.hs
module Native.Artifacts.Game where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Native.Platform ()
import Native.Stage.ChangeCursor ()
import Native.Stage.DetermineInput ()
import Native.Stage.Prerequisites ()
import Native.Stage.SystemLibraries ()
import qualified Heroes.Requisites                         as RQ
import qualified Native.Stage.DrawEverything_              as D
import qualified Native.Stage.IssueSoundCommands_          as S
import qualified Native.Stage.PrepareForDrawing_           as X
import qualified Stage.Animation                           as A
import qualified Stage.Blackbox                            as B
import qualified Stage.ChangeCursor                        as C
import qualified Stage.DetermineInput                      as I
import qualified Stage.Loading                             as L
import qualified Stage.LoadingThread                       as LT
import qualified Stage.Prerequisites                       as PR
import qualified Stage.SystemLibraries                     as SL
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

main' :: IO ()
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
    X.with (X.Deps {..}) $ \prepareForDrawing_     ->
    D.with (D.Deps {..}) $ \drawEverything_        ->
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
          X.Out {..} <- prepareForDrawing_ (X.In {..})
          --------------------------------------------
          wishLoaded          (L.WishIn {..})
          issueSoundCommands_ (S.In {..})
          changeCursor        (C.In {..})
          drawEverything_     (D.In {..})
          --------------------------------
          again
  --
  do
    putStrLn "----------------"
    putStrLn "-- exiting... --"
    putStrLn "----------------"
