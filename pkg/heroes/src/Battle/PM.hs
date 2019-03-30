module Battle.PM where

import Heroes

data Node = Node {
  currentPlacing :: Placing,
  points :: Int
} deriving (Eq, Show)

data Marker -- path-finding marker
  = Attack {
    attackerPlacing :: Placing,
    bearing :: Bearing,
    hit :: Hex
  } deriving (Eq, Show)
