module Plover where

import Dictionary
import Keys
import Steno.Alphabet
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R

plover =
  [Entry "{*+}" [stk Hash]
  ,Entry "{*($c)}" [stk R.D <> stk R.Z <> stk Hash]
  ]
