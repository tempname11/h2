module Stage.Core (
  with,
  Deps (..),
  In (..),
  Out(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle
import Battle.Monad.Utils
import Battle.Rules
import Battle.Setup
import Battle.Movement
import Heroes
import Heroes.Aux
import Heroes.AAI                                        (AIQuery(..))
import Heroes.AAI                                        (AIResult(..))
import Heroes.UI
import qualified Battle.AM                                 as AM
import qualified Common.Hot                                as Hot
import qualified Heroes.Cell                               as Cell
import qualified Heroes.ControlMap                         as ControlMap
import qualified Heroes.Input                              as Input
import qualified Heroes.Placing                            as Placing
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Set                                  as S
import qualified Data.Map.Strict                           as M
import qualified Data.Vector                               as V
import Safe                                              (atMay)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  initialBattle :: Battle,
  setup :: Setup,
  queryAI :: IO (Maybe AIResult),
  askAI :: Maybe AIQuery -> IO ()
}

data In = In {
  isActive0 :: Bool,
  fullInput :: Input.Full
}

data Out = Out {
  darkHexes :: V.Vector Hex,
  exit :: Bool,
  extraColor :: FighterId -> Maybe Color,
  intent :: Maybe Annotation,
  lightHexes :: V.Vector Hex,
  update :: AM.Update
}

--------------------------------------------------------------------------------

with :: Deps -> ((In -> IO Out) -> IO a) -> IO a
with deps next = do
  let Deps {..} = deps
  ref <- newIORef (Data {
    current = Current (setup, initialBattle),
    pastBattles = [],
    futureBattles = []
  })
  next $ \in_ -> do
    d0 <- readIORef ref
    aiMoves <- queryAI
    let (out, q, d1) = core deps aiMoves in_ d0
    writeIORef ref d1
    askAI q
    return out

--------------------------------------------------------------------------------

data Data = Data {
  pastBattles :: [Battle],
  futureBattles :: [Battle],
  current :: Current (Setup, Battle)
} deriving (Generic)

--------------------------------------------------------------------------------

-- XXX ugly as hell :)
battlefieldHexesMemo :: Current (Setup, a) -> V.Vector Hex
battlefieldHexesMemo =
  Hot.forget .
  Hot.memo1 (V.fromList . S.toList . view #field . fst . Hot.this) .
  Hot.currently

core :: Deps -> Maybe AIResult -> In -> Data -> (Out, Maybe AIQuery, Data)
core (Deps {..}) aiMoves (In {..}) data0 = (Out {..}, aiQuery, data1)
  where
  Input.Full {..} = fullInput
  --
  isHuman = case current0' #?. currentPlayerType of
    Just Human -> True
    _ -> False
  --
  spellIndex =
    (guard (keyDown Input.Key'1) >> Just 0)
    <|>
    (guard (keyDown Input.Key'2) >> Just 1)
    <|>
    (guard (keyDown Input.Key'3) >> Just 2)

  --
  aiQuery =
    if not isHuman
    then Just (AIQuery (data0 ^. #current))
    else Nothing
  acceptable = acceptableMovesMemo current0
  moves = do
    guard isActive0
    if not isHuman
    then aiMoves >>= \case
      AIResult'OK ms -> Just ms
      AIResult'Fail -> Nothing -- XXX
      AIResult'Pending -> Nothing
    else
      if acceptable == [EOM]
      then Just [EOM]
      else
        case spellIndex of
          Just i -> do
            let
              isSpell = \case
                SpellSelected {} -> True
                _ -> False
              spellMoves = filter isSpell acceptable
            case spellMoves `atMay` i of
              Just m -> Just [m]
              Nothing -> Nothing
          Nothing ->
            if mouseDown Input.LMB
              then view _2 <$> payload
              else Nothing
  --
  intent =
    case aiMoves of
      Just (AIResult'Pending) -> Just Annotation'Thinking
      _ -> view _1 <$> payload
  --
  battleField = setup ^. #field
  movementHexes =
    if isActive0 && isHuman
    then (M.keys . \(M p _) -> p) (aux (current data1) ^. #movementHexes)
    else []
  destinationHexes =
    if isActive0 && isHuman
    then
      case join $ view _3 <$> payload of
        Just p -> Placing.visit p
        Nothing -> []
    else []
  lightHexes = battlefieldHexesMemo current0
  -- XXX memoize?
  darkHexes = V.fromList (movementHexes <> destinationHexes)
  exit = quitEvent || keyUp ControlMap.exit
  --
  payload = do
    guard isActive0
    segment <- hoveredSegment
    payloadAt segment aux0

  Data {
    pastBattles = pastBattles0,
    futureBattles = futureBattles0,
    current = current0
  } = data0
  --
  aux0 = aux current0
  (Current current0') = current0
  battle0 = snd current0'
  --
  hoveredSegment = case mouseXY of
    Nothing -> Nothing
    Just nbc' -> do
      let segment = Cell.toSegment ((<§>) nbc' .-. fieldCenter)
          hex     = segment ^. #hex
          inField = hex `elem` battleField
      guard inField
      return segment

  (update, data1) =
    let
      result = moves >>= \moves' -> do
        -- USEFUL traceShowM moves'
        return $
          rightIsJust (current0' #%!&. for_ moves' makeMove)
          & presumeJust "If this is Nothing, we must have done something \
          \ wrong while calculating the moves in Aux. It's better to \
          \ crash here than ignore the error silently."
      normal = case result of
        Nothing -> (AM.Normal [], data0)
        Just (battle1, as, _) -> (
          AM.Normal as,
          Data {
            current = Current (setup, battle1),
            pastBattles =
              case (battle0 ^. #phase, isHuman) of
                (Phase'Initial {}, True) -> battle0 : pastBattles0
                _ -> pastBattles0,
            futureBattles = []
          })
    in if
      | not isActive0 -> normal
      | keyDown ControlMap.reset -> (
          AM.JumpTo (Some initialBattle),
          Data {
            current = Current (setup, initialBattle),
            pastBattles = [],
            futureBattles = reverse pastBattles0 <> [battle0] <> futureBattles0
          }
        )
      | keyDown ControlMap.intoPast ->
        case pastBattles0 of
          b : bs -> (
              AM.JumpTo (Some b),
              Data {
                current = Current (setup, b),
                pastBattles = bs,
                futureBattles = battle0 : futureBattles0
              }
            )
          [] -> normal
      | keyDown ControlMap.intoFuture ->
        case futureBattles0 of
          b : bs -> (
              AM.JumpTo (Some b),
              Data {
                current = Current (setup, b),
                futureBattles = bs,
                pastBattles = battle0 : pastBattles0
              }
            )
          [] -> normal
      | otherwise -> normal
  --
  hoveredFighter = do
    guard isActive0
    hex <- view #hex <$> hoveredSegment
    join $ current0' #?. whoIsOn hex

  extraColor fyr =
    if isActive0 && isHuman
    then if fyr `elem` (aux0 ^. #selectableFighters)
         then Just $
           if (hoveredFighter == Just fyr)
           then cyan
           else yellow
         else Nothing
    else Nothing
