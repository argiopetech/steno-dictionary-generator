module Plover where

import Dictionary
import Keys
import Sounds
import Steno.Alphabet
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R

plover =
  let able = aa <> b' <> l'
      nable = n <> able
      dable = d <> able
  in entries
  [entry "{*+}"    [stk Hash]
  ,entry "{*($c)}" [stk R.D <> stk R.Z <> stk Hash]
--  ,entry "" [stk R.Z <> stk Hash]
  ,entry "{^}{-|}" [k <> p <> a]
  ,entry "{}{-|}"  [k <> p <> a <> star]
  ,entry "{^ ^}"   [stk L.S <> stk R.P] -- This is also in control keys
  ,entry "{}{^}"   [stk L.S <> stk R.D]
  ,entry "{^}"     [t <> k <> l' <> s']

  -- Modes
  ,entry "{MODE:CAPS}" [k <> a <> fvs' <> star]
  ,entry "{MODE:TITLE}" [t <> ii <> fvs' <> star]
  ,entry "{MODE:LOWER}" [l <> oe <> fvs' <> star]

  -- These three need special space handling, I think
  ,entry "{^ ^}{MODE:CAMEL}" [k <> m <> fvs' <> star]
  ,entry "{^ ^}{MODE:SNAKE}" [s <> n <> fvs' <> star]
  ,entry "{^ ^}{MODE:SNAKE}{MODE:CAPS}" [s <> n <> a <> fvs' <> star]

  ,entry "{MODE:RESET}" [r <> ee <> fvs' <> star]


  -- Enable/disable
  ,entry "{}"      [nable]
  ,entry "{}"      [dable]
  ,entry "{PLOVER:TOGGLE_DICT:+csharp.json}"
         [nable, c <> sh']
  ,entry "{PLOVER:TOGGLE_DICT:-csharp.json}"
         [dable, c <> sh']
  ,entry "{PLOVER:TOGGLE_DICT:+emacs.json}"
         [nable, m <> a <> x']
  ,entry "{PLOVER:TOGGLE_DICT:-emacs.json}"
         [dable, m <> a <> x']
  ,entry "{PLOVER:TOGGLE_DICT:+haskell.json}"
         [nable, h <> a <> fvs' <> k']
  ,entry "{PLOVER:TOGGLE_DICT:-haskell.json}"
         [dable, h <> a <> fvs' <> k']
  ]
