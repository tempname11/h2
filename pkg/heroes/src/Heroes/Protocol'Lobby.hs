module Heroes.Protocol'Lobby where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Binary                                       (Binary)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data MatchInfo = MatchInfo {
  name :: Text
} deriving (Generic)

data Request
  = Request'ListMatches
  deriving (Generic)

data Response
  = Response'Matches [MatchInfo]
  deriving (Generic)

instance Binary MatchInfo
instance Binary Request
instance Binary Response
