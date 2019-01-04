module Common.Optics (
  module Control.Lens,
  module Linear,
  module Common.TH,
  module Extra.Field.Optics,
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Control.Lens                                      ((+=))
import Control.Lens                                      ((<+=))
import Control.Lens                                      ((-=))
import Control.Lens                                      ((%=))
import Control.Lens                                      ((<%=))
import Control.Lens                                      ((.=))
import Control.Lens                                      ((.~))
import Control.Lens                                      ((^.))
import Control.Lens                                      (at)
import Control.Lens                                      (ix)
import Control.Lens                                      (assign)
import Control.Lens                                      (use)
import Control.Lens                                      (view)
import Control.Lens                                      (over)
import Control.Lens                                      (set)
import Control.Lens                                      (zoom)
import Control.Lens                                      (_1)
import Control.Lens                                      (_2)
import Control.Lens                                      (_3)
import Control.Lens                                      (_4)
import Control.Lens                                      (_5)
import Control.Lens                                      (_6)
import Control.Lens                                      (_7)
import Control.Lens                                      (_8)
import Control.Lens                                      (_9)

import Linear                                            (_x)
import Linear                                            (_y)
import Linear                                            (_z)
import Linear                                            (_w)

import Common.TH                                         (makeShorthands)
import Common.TH                                         (declareShorthand)

import Extra.Field.Optics                                (by)
import Extra.Field.Optics                                (useMay)
import Extra.Field.Optics                                (setMay)
import Extra.Field.Optics                                (overMay)
import Extra.Field.Optics                                (viewMay)
import Extra.Field.Optics                                ((^.?))
import Extra.Field.Optics                                ((%~?))
import Extra.Field.Optics                                ((.~?))
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
