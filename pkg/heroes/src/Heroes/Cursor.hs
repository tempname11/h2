module Heroes.Cursor (
  Type (..),
  Pointing (..),
  cursorMeta,
  fromIntent
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Heroes.Aux                                        (Annotation(..))
import qualified Heroes.Bearing                            as Bearing
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Pointing
  = Pointing'Up
  | Pointing'Down
  | Pointing'To Bearing
  deriving (Generic, Eq, Ord, Show)

data Type
  = Normal
  | Question
  | Blocked
  | Run
  | Fly
  | Sword Pointing
  | Arrow
  | BrokenArrow
  | Helmet
  | Heal
  | Sacrifice
  | Portal
  | Catapult
  | Hourglass
  | Spellbook
  deriving (Generic, Eq, Ord, Show)

-- XXX DeriveAnyClass (since ghc 8.2)
instance GEnum Pointing
instance GEnum Type

-- magic "i-j" names, but whatever
cursorMeta :: Type -> (String, V2 CInt)
cursorMeta = \case
  Normal -> ("0-0", V2 0 0)
  Question -> ("0-1", V2  6 13)
  Blocked -> ("0-2", V2 12 13)
  Run -> ("0-3", V2 11 11)
  Fly -> ("0-4", V2 13 9)
  Sword Pointing'Up -> ("0-5", V2 6 3)
  Sword (Pointing'To NE) -> ("0-6", V2 20 1)
  Sword (Pointing'To E) -> ("0-7", V2 31 6)
  Sword (Pointing'To SE) -> ("0-8", V2 21 21)
  Sword Pointing'Down -> ("0-9", V2 6 31)
  Sword (Pointing'To SW) -> ("0-10", V2 0 22)
  Sword (Pointing'To W) -> ("0-11", V2 0 5)
  Sword (Pointing'To NW) -> ("0-12", V2 0 1)
  Arrow -> ("0-13", V2 13 11)
  BrokenArrow -> ("0-14", V2 13 2)
  Helmet -> ("0-15", V2 12 8)
  Heal -> ("0-16", V2 12 9)
  Sacrifice -> ("0-17", V2 12 12)
  Portal -> ("0-18", V2 13 15)
  Catapult -> ("0-19", V2 14 10)
  Hourglass -> ("1-2", V2 9 11)
  Spellbook -> ("2-0", V2 16 15) -- XXX animation

fromIntent :: Maybe Annotation -> Type
fromIntent = \case
  Just Annotation'Running -> Run
  Just Annotation'Pondering -> Question
  Just (Annotation'MeleeFrom b) -> Sword (Pointing'To $ Bearing.opposite b)
  Just Annotation'Range -> Arrow
  Just Annotation'Selecting -> Normal
  Nothing -> Normal
