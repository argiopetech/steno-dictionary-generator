module Punctuation where

import Dictionary
import Keys
import Steno.Alphabet
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R


coding =
  [Entry "." [stk L.T <> stk L.K <> stk O <> stk R.T]
  ,Entry "{^.^}" [stk L.P <> stk R.P]
  ,Entry "{^\\\\^}" [stk L.S <> stk L.H <> stk L.R <> stk A]
  ,Entry "/" [stk L.T <> stk L.K <> stk E <> stk U <> stk R.Fvs]
  ,Entry "{^/^}" [stk O <> stk E <> stk U]
  ,Entry "*" [stk L.T <> stk A <> stk O <> stk E <> stk U
           <> stk R.P <> stk R.L <> stk R.Z]
  ,Entry "{^*^}" [stk L.S <> stk L.T <> stk L.R <> stk R.Fvs <> stk R.B <> stk R.G]
  ]

punctuation =
  let equal = stk L.K <> stk L.W <> stk L.R <> stk L.R
  in [Entry "{,}" [stk L.K <> stk L.W <> stk R.B <> stk R.G]
     ,Entry "{.}" [stk L.T <> stk L.P <> stk R.P <> stk R.L]
     ,Entry "{?}" [stk L.K <> stk L.W <> stk R.P <> stk R.L]
     ,Entry "{!}" [stk L.T <> stk L.P <> stk R.B <> stk R.G]
     ,Entry "{~|\\\"^}" [stk L.K <> stk L.W <> stk R.G <> stk R.S]
     ,Entry "{^~|\\\"}" [stk L.K <> stk L.R <> stk R.G <> stk R.S]
     ,Entry "{~|\'^}" [stk L.S <> stk L.K <> stk R.G <> stk R.S]
     ,Entry "{^~|\'}" [stk L.S <> stk L.R <> stk R.G <> stk R.S]
     ,Entry "{~|(^}" [stk L.P <> stk L.R <> stk E <> stk R.P <> stk R.B]
     ,Entry "{^~|)}" [stk L.P <> stk L.R <> stk E <> stk R.P <> stk R.B <> stk R.T]
     ,Entry "{~|{^}" [stk L.K <> stk L.R <> stk E <> stk R.P <> stk R.B]
     ,Entry "{^~|}}" [stk L.K <> stk L.R <> stk E <> stk R.P <> stk R.B <> stk R.T]
     ,Entry "{~|[^}" [stk L.S <> stk L.R <> stk E <> stk R.P <> stk R.B]
     ,Entry "{^~|]}" [stk L.S <> stk L.R <> stk E <> stk R.P <> stk R.B <> stk R.T]
     ,Entry "{^-^}"  [stk L.H <> stk R.R]
     ,Entry "{^--^}" [stk L.H <> stk R.R <> stk R.B]
     ,Entry "=" [equal]
     ,Entry "{^=^}" [equal <> stk R.S]
     ,Entry "==" [equal, equal]
     ]
