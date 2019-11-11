module Punctuation where

import Dictionary
import Keys
import Steno.Alphabet
import Stroke
import Sounds

import qualified Keys.Left as L
import qualified Keys.Right as R

dot = Entry "." [d <> o <> t']

coding =
  [dot
  ,Entry "{^.^}" [p <> p']
  ,Entry "{^,^}" [w <> b']
  ,Entry "{^\\\\^}" [s <> l <> a]
  --,Entry "/" [d <> i <> fvs'] -- div; I don't like this
  ,Entry "{^/^}" [stk O <> stk E <> stk U]
  ,Entry "{&*}" [t <> ii <> m' <> z']
  ,Entry "{^*^}" [s <> t <> r <> fvs' <> b' <> g']
  ,Entry "<>"    [p <> e <> n' <> d']
  ,Entry "{^}^{^}" [x <> p']
  ,Entry "{^}#{^}" [h <> a <> sh' <> stk Star]
  ]

punctuation =
  let equal = k <> w <> l
  in [Entry "{,}" [stk L.K <> stk L.W <> stk R.B <> stk R.G]
     ,Entry "{.}" [stk L.T <> stk L.P <> stk R.P <> stk R.L]
     ,Entry "{!}" [stk L.K <> stk L.W <> stk R.P <> stk R.L]
     ,Entry "{?}" [stk L.T <> stk L.P <> stk R.B <> stk R.G]
     ,Entry "{~|\\\"^}" [stk L.K <> stk L.W <> stk R.G <> stk R.S]
     ,Entry "{^~|\\\"}" [stk L.K <> stk L.R <> stk R.G <> stk R.S]
     ,Entry "{~|\'^}"   [stk L.S <> stk L.W <> stk R.G <> stk R.S]
     ,Entry "{^~|\'}"   [stk L.S <> stk L.R <> stk R.G <> stk R.S]
     ,Entry "{~|(^}"    [stk L.P <> stk L.R <> stk E <> stk R.P <> stk R.B]
     ,Entry "{^~|)}"    [stk L.P <> stk L.R <> stk E <> stk R.P <> stk R.B <> stk R.T]
     ,Entry "{~|\\\\{^}" [stk L.K <> stk L.R <> stk E <> stk R.P <> stk R.B]
     ,Entry "{^~|\\\\}}" [stk L.K <> stk L.R <> stk E <> stk R.P <> stk R.B <> stk R.T]
     ,Entry "{~|[^}" [stk L.S <> stk L.R <> stk E <> stk R.P <> stk R.B]
     ,Entry "{^~|]}" [stk L.S <> stk L.R <> stk E <> stk R.P <> stk R.B <> stk R.T]
     ,Entry "{^-^}"  [stk L.H <> stk R.R]
     ,Entry "{^--^}" [stk L.H <> stk R.R <> stk R.B]
     ,Entry "{^_^}"  [s, k, r]
     ,Entry "="      [equal]
     ,Entry "{^=^}"  [equal <> stk R.S]
     ,Entry "=="     [equal, equal]
     ,Entry "<{^}"   [w <> a <> k']
     ,Entry "{^}>"   [w <> a <> k' <> t']
     ,Entry "{^~^}"  [t <> l <> d']
     ,Entry ":"      [s <> t <> p <> h <> fvs' <> p' <> l' <> t']
     ,Entry "$"   [stk R.D <> stk Hash]
     ,Entry "&"      [m <> p' <> stk Star]
     ]
