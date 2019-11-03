module Plover where

import Dictionary
import Keys
import Sounds
import Steno.Alphabet
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R

plover =
  [Entry "{*+}" [stk Hash]
  ,Entry "{*($c)}" [stk R.D <> stk R.Z <> stk Hash]
--  ,Entry "" [stk R.Z <> stk Hash]
  ,Entry "{^}{-|}" [k <> p <> a]
  ,Entry "{}{-|}" [k <> p <> a <> stk Star]
  ,Entry "{^ ^}" [stk L.S <> stk R.P]
  ,Entry "{}{^}" [stk L.S <> stk R.D]
  ,Entry "{^}" [t <> k <> l' <> s']
  ]
