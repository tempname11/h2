{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Common.Fields where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Data.Generics.Product                             (field')
import Data.Generics.Product                             (HasField')
import GHC.OverloadedLabels                              (IsLabel(..))
import Prelude                                           (Functor)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

instance (
    HasField' l s a,
    Functor f,
    lens ~ ((a -> f a) -> s -> f s)
  ) => IsLabel l lens where
  fromLabel _ = field' @ l
