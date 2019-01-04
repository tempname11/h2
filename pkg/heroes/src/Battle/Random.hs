module Battle.Random where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Battle
import Battle.Monad.Utils
import Battle.Setup
import Heroes
import Heroes.Internal
import qualified Heroes.H3                                 as H3
import Battle.Rules                                      (spawns)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Test.QuickCheck                                   (Gen)
import Test.QuickCheck                                   (choose)
import Test.QuickCheck                                   (Arbitrary)
import Test.QuickCheck                                   (arbitrary)
import Test.QuickCheck                                   (arbitraryBoundedEnum)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data F = F Creature FighterAttr
instance Arbitrary F where
  arbitrary = do
    creature <- arbitraryBoundedEnum
    hex <- do
      d <- choose (-4, 4)
      q <- choose (-4, 4)
      return $ Hex d q
    speed <- choose (1, 7)
    attack <- choose (1, 7)
    defence <- choose (1, 7)
    facing <- arbitraryBoundedEnum
    t <- choose (1, 2)
    let pc = if H3.wide creature then Wide else Narrow
        placing = pc hex
        attr = FighterAttr {
          team = Team t,
          placing,
          speed,
          attack, 
          defence,
          abilities = empty,
          facing
        }
    return $ F creature attr


spawn :: Int -> (Setup, Battle) -> Gen (Setup, Battle)
spawn 0 (setup, battle) = return (setup, battle)
spawn n (setup, battle) = do
  F c attr <- arbitrary
  case (setup, battle) #?! spawns c attr of
    Just battle' -> spawn (n - 1) (setup, battle')
    Nothing -> spawn n (setup, battle)
