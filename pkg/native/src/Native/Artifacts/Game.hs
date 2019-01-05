-- XXX marked for deletion in favor of Heroes/Game.hs
module Native.Artifacts.Game where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Native
import Native.Platform ()
import qualified Heroes.Requisites                         as RQ
import qualified Native.Prerequisites                      as PR
import qualified Native.Stage.ChangeTheDamnCursor_         as U
import qualified Native.Stage.DetermineFullInput_          as E
import qualified Native.Stage.DrawEverything_              as D
import qualified Native.Stage.IssueSoundCommands_          as S
import qualified Native.Stage.PrepareForDrawing_           as X
import qualified Native.SystemLibraries                    as SL
import qualified Stage.Animation                           as A
import qualified Stage.Blackbox                            as B
import qualified Stage.Loading                             as L
import qualified Stage.LoadingThread                       as LT
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
    --------------------------------------------------
    L.with (L.Deps {..}) $ \queryLoaded wishLoaded  ->
    B.with (B.Deps {..}) $ \blackbox                ->
    A.with (A.Deps {..}) $ \animation               ->
    X.with (X.Deps {..}) $ \prepareForDrawing_      ->
    E.with (E.Deps {..}) $ \determineFullInput_     ->
    U.with (U.Deps {..}) $ \changeTheDamnCursor_    ->
    S.with (S.Deps {..}) $ \issueSoundCommands_     ->
    D.with (D.Deps {..}) $ \drawEverything_         ->
    --------------------------------------------------
    fix $ \again -> do
        ----------------------------------------
        L.QueryOut {..}  <- queryLoaded
        E.Out {..}       <- determineFullInput_
        B.Out {exit, ..} <- blackbox (B.In {..})
        ----------------------------------------
        unless exit $ do
          --------------------------------------------
          A.Out {..} <- animation          (A.In {..})
          X.Out {..} <- prepareForDrawing_ (X.In {..})
          --------------------------------------------
          wishLoaded           (L.WishIn {..})
          issueSoundCommands_  (S.In {..})
          changeTheDamnCursor_ (U.In {..})
          drawEverything_      (D.In {..})
          --------------------------------
          again
  --
  do
    putStrLn "----------------"
    putStrLn "-- exiting... --"
    putStrLn "----------------"
