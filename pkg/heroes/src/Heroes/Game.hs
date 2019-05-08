module Heroes.Game where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Platform                                   (Platform)
import qualified Heroes.ActionQueue                        as ActionQueue         
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
      rootRef <- do
        initial <- rootProv ^. #new
        newIORef (Just initial)
      actionQ <- ActionQueue.new
      --
      fix $ \again -> do
        dispatch <- ActionQueue.useDispatch actionQ
        mroot <- do
          x <- readIORef rootRef >>= \case
            Nothing -> return Nothing
            Just root ->
              ActionQueue.take1 actionQ >>= \case
                Nothing -> return (Just root)
                Just action ->
                  (rootProv ^. #action) action root
          --
          writeIORef rootRef x
          return x
        --
        case mroot of
          Nothing -> return ()
          Just root -> do
            load
            L.QueryOut {..} <- queryLoaded
            I.Out {..} <- determineInput
            Root.Out {..} <- (rootProv ^. #run) (Root.In {..}) root
            --
            wishLoaded (L.WishIn {..})
            playSounds (SND.In {..})
            changeCursor (WND.In {..})
            draw drawCallback
            waitForVsync
            again
