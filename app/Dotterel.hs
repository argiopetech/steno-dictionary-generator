module Dotterel where

import Dictionary
import Keys
import Sounds
import Steno.Numbers
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R

import Text.Printf


dotterel = entries $
  [entry "{^}{IME:EDITOR_ACTION}{MODE:RESET}{^}{-|}" [r <> r' <> b']
  ,entry "{RETRO:UNDO}" [stk Star]]
