module Native.Artifacts.Game where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Common.IO
import Heroes
import qualified Heroes.Requisites                         as RQ
import qualified Native.LoadingThread                      as LT
import qualified Native.Prerequisites                      as PR
import qualified Native.Stage.ChangeTheDamnCursor_         as U
import qualified Native.Stage.DetermineFullInput_          as E
import qualified Native.Stage.DrawEverything_              as D
import qualified Native.Stage.IssueSoundCommands_          as S
import qualified Native.Stage.Loading                      as L
import qualified Native.Stage.PrepareForDrawing_           as X
import qualified Native.SystemLibraries                    as SystemLibraries
import qualified Stage.Animation                           as A
import qualified Stage.Blackbox                            as B
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

main' :: IO ()
main' = do
  do
    putStrLn "--------------------"
    putStrLn "-- Starting up... --"
    putStrLn "--------------------"
  --
  let noDeps = ()
  SystemLibraries.with $ \(SystemLibraries.Prov   {..}) ->
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
    ----------------------------------------
    fix $ \again -> do
        -------------------------------------------
        L.QueryOut {..}  <- queryLoaded
        E.Out {..}       <- determineFullInput_
        B.Out {exit, ..} <- blackbox (B.In {..})
        ----------------
        unless exit $ do
          --------------------------------------------
          A.Out {..} <- animation          (A.In {..})
          X.Out {..} <- prepareForDrawing_ (X.In {..})
          ------------------------------------
          wishLoaded           (L.WishIn {..})
          issueSoundCommands_  (S.In {..})
          changeTheDamnCursor_ (U.In {..})
          drawEverything_      (D.In {..})
          ----------------------------
          again
  --
  do
    putStrLn "----------------"
    putStrLn "-- exiting... --"
    putStrLn "----------------"
