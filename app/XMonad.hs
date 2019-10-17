module XMonad where

import Dictionary
import Keys
import Sounds
import Steno.Numbers
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R

import Text.Printf


switchDesktops = stks [s <> w]
moveWindow = stks [s <> w <> stk Star]

xmonad = entries $
  [entry "{#}" [switchDesktops]
  ,entry "{#}" [moveWindow]] ++ map go numbers
  where go (Entry n s) =
          [Entry (printf "{#super(%s)}{^}" n) (switchDesktops : s)
          ,Entry (printf "{#super(shift(%s))}{^}" n) (moveWindow : s)]
