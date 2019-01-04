module Battle.Example where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle
import Battle.Setup
import Heroes
import Heroes.Internal
import Heroes.H3.Misc
import qualified Heroes.H3                                 as H3
import qualified Heroes.Hex                                as Hex
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import qualified Data.Map.Strict                           as M
import qualified Data.Set                                  as S
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

fromFighters :: M.Map FighterId FighterAttr -> (Setup, Battle)
fromFighters fighters = (setup, battle)
  where
  setup = Setup {
    field = Hex.standardField 7 5,
    participants = M.fromList [
      (red, TeamAttr { playerType = Human }),
      (blue, TeamAttr { playerType = AI })
    ]
  }
  --
  orderFrom :: Turn -> [Team] -> [Team] -> Stream (Team, Turn)
  orderFrom turn [] again = orderFrom (turn + 1) again again
  orderFrom turn (p : ps) again = Cons (p, turn) (orderFrom turn ps again)
  battle = Battle {
    fighters,
    obstacles = M.fromList [
      (
        makeObstacleId Obstacle'0 1,
        ObstacleAttr {
          multiplacing = Multiplacing (Hex (-1) (-1)) (obstacleDiffs Obstacle'0)
        }
      )
    ],
    bodies = empty,
    order = orderFrom (-1) [] (M.keys (setup ^. participants_)),
    phase = Phase'Initial
  }

red :: Team
red  = Team 1

blue :: Team
blue = Team 2

zero :: (Setup, Battle)
zero = fromFighters empty

one :: (Setup, Battle)
one = fromFighters $ M.fromList [
    (makeFighterId H3.GreenDragon 1, f1),
    (makeFighterId H3.Dwarf 2, f2)
  ]
  where
  f1 = FighterAttr {
    team = red,
    speed = 5,
    attack = 4,
    defence = 3,
    abilities = S.fromList [Ability'Ranged, Ability'Flight],
    placing = Wide $ Hex (-2) (-2),
    facing = East
  }
  f2 = FighterAttr {
    team = blue,
    speed = 2,
    attack = 4,
    defence = 3,
    abilities = empty,
    placing = Narrow $ Hex 2 2,
    facing = West
  }

