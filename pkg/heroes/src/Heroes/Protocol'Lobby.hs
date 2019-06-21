module Heroes.Protocol'Lobby where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
import Battle                                            (Battle)
import Battle.Setup                                      (Setup)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import GHC.TypeLits                                      (Symbol)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

newtype ID (a :: Symbol) = ID Int 
  deriving (Generic, Eq, Ord, Show)

data U
  = U'ListCreatedMatches {
    r'ID :: ID "Request"
  }
  | U'CreateMatch {
    r'ID :: ID "Request"
  }
  | U'JoinMatch {
    r'ID :: ID "Request",
    m'ID :: ID "Match"
  }
  deriving (Generic)

data D
  = D'CreatedMatchesAre {
    r'ID :: ID "Request",
    matches :: [ID "Match"]
  }
  | D'MatchWasCreated {
    r'ID :: ID "Request",
    m'ID :: ID "Match"
  }
  | D'NoSuchMatch {
    r'ID :: ID "Request"
  }
  | D'MatchWasStarted {
    r'ID :: ID "Request",
    m'ID :: ID "Match",
    team :: Team,
    setup :: Setup,
    initialBattle :: Battle
  }
  deriving (Generic)

instance Binary (ID a)
instance Binary U
instance Binary D
