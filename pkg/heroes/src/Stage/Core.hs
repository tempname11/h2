module Stage.Core (
  with,
  Deps (..),
  In (..),
  Out(..),
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle
import Battle.AI.Search
import Battle.Monad.Utils
import Battle.Rules
import Battle.Setup
import Battle.Movement
import Common.Hot
import Heroes
import Heroes.Aux
import Heroes.UI
import qualified Heroes.Cell                               as Cell
import qualified Heroes.Placing                            as Placing
import qualified Heroes.ControlMap                         as ControlMap
import qualified Battle.AM                                 as AM
import qualified Heroes.Input                              as Input
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Set                                  as S
import qualified Data.Map.Strict                           as M
import Safe                                              (atMay)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Deps = Deps {
  initialBattle :: Battle,
  setup :: Setup
}

data In = In {
  isActive0 :: Bool,
  fullInput :: Input.Full
}

data Out = Out {
  darkHexes :: [Hex],
  exit :: Bool,
  extraColor :: FighterId -> Maybe Color,
  ghostPlacing :: Maybe Placing,
  intent :: Maybe Annotation,
  lightHexes :: [Hex],
  update :: AM.Update
}

--------------------------------------------------------------------------------

with :: Deps -> ((In -> IO Out) -> IO a) -> IO a
with deps next = do
  let Deps {setup, initialBattle} = deps
  ref <- newIORef (Data {
    current = Current (setup, initialBattle),
    pastBattles = [],
    futureBattles = []
  })
  next $ \in_ -> do
    d0 <- readIORef ref
    let (out, d1) = core deps in_ d0
    writeIORef ref d1
    return out

--------------------------------------------------------------------------------

data Data = Data {
  pastBattles :: [Battle],
  futureBattles :: [Battle],
  current :: Current (Setup, Battle)
}

--------------------------------------------------------------------------------

core :: Deps -> In -> Data -> (Out, Data)
core (Deps {..}) (In {..}) data0 = (Out {..}, data1)
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
  moves = do
    guard isActive0
    if not isHuman
    then ai current0'
    else case spellIndex of
      Just i -> do
        let
          isSpell = \case
            SpellSelected {} -> True
            _ -> False
          spellMoves = filter isSpell (acceptableMoves (setup, battle0))
        case spellMoves `atMay` i of
          Just m -> Just [m]
          Nothing -> Nothing
      Nothing ->
        if mouseDown Input.LMB
          then snd <$> payload
          else Nothing
  --
  intent = fst <$> payload
  --
  ghostHex = do
    guard isActive0
    view hex_ <$> hoveredSegment
  --
  ghostPlacing = Placing.teleport <$> ghostHex <*> placing
  battleField = setup ^. field_
  placing = current0' #?. currentFighterPlacing
  movementHexes =
    if isHuman
    then (M.keys . \(M p _) -> p) (aux (current data1) ^. movementHexes_)
    else []
  ghostHexes =
    if isHuman
    then maybe [] Placing.visit ghostPlacing
    else []
  lightHexes = S.toList battleField
  darkHexes = movementHexes <> ghostHexes
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
          hex     = segment ^. hex_
          inField = hex `elem` battleField
      guard inField
      return segment

  (update, data1) =
    let
      result = moves >>= \moves' -> do
        -- traceShowM moves'
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
              case (battle0 ^. phase_, isHuman) of
                (Phase'Initial {}, True) -> battle0 : pastBattles0
                _ -> pastBattles0,
            futureBattles = []
          })
    in if
      | not isActive0 -> normal
      | keyDown ControlMap.reset -> (
          AM.JumpTo initialBattle,
          Data {
            current = Current (setup, initialBattle),
            pastBattles = [],
            futureBattles = reverse pastBattles0 <> [battle0] <> futureBattles0
          }
        )
      | keyDown ControlMap.intoPast ->
        case pastBattles0 of
          b : bs -> (
              AM.JumpTo b,
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
              AM.JumpTo b,
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
    hex <- view hex_ <$> hoveredSegment
    join $ current0' #?. whoIsOn hex

  extraColor fyr =
    if isActive0 && isHuman
    then if fyr `elem` (aux0 ^. selectableFighters_)
         then Just $
           if (hoveredFighter == Just fyr)
           then cyan
           else yellow
         else Nothing
    else Nothing
