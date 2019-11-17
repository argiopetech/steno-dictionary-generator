module Haskell where

import Dictionary
import Keys
import Sounds
import Steno.Alphabet
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R
  

haskell =
  entries
  [entry "Int" [n <> t']
  ,entry "Bool" [b <> ew <> l']

  ,entry "&&" [n' <> d']
  ,entry "||" [o <> r']

  ,entry  "/=" [n <> k <> w <> l]
  ,entry  "->" [l <> a <> m' <> d']
  
  ,entry "<>"    [p <> e <> n' <> d']
  ]
