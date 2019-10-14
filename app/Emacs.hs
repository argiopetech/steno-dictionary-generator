module Emacs where

import Dictionary
import Keys
import Sounds
import Steno.Alphabet
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R

emacs = entries
  [entry "{#control(slash)}" [stk Star <> stk Hash]
  ]
