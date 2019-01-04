module Battle.PM where

import Heroes

data Node = Node Hex Int
  deriving (Eq, Show)

data Marker -- path-finding marker
  = Attack Bearing Hex
  deriving (Eq, Show)
