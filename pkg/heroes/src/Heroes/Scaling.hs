module Heroes.Scaling where

import Common

scalingFactor :: CInt
scalingFactor = 1

rescaled :: Functor f => f CInt -> f CInt -- works with vectors and points both
rescaled = fmap (* scalingFactor)

antirescaled :: Functor f => f CInt -> f CInt -- same
antirescaled = fmap (`div` scalingFactor)

