module ControlKeys where

import Dictionary
import Keys
import Steno.Alphabet
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R

leftArrow = stk R.Fvs

arrows =
  [Entry "Left" [leftArrow]
  ,Entry "Right" [stk R.T]
  ,Entry "Up" [stk R.L]
  ,Entry "Down" [stk R.P]]

home = Entry "Home" [stk R.P <> stk R.B]
end = Entry "End" [stk R.L <> stk R.G]
space = Entry "Space" [stk L.S <> stk L.P]
enter = Entry "Return" [stk L.R <> stk L.T]
backspace = Entry "Backspace" [stk R.D]
tab = Entry "Tab" [stk L.T <> stk A]

specialKeys =
  [tab
  ,Entry "Escape" [stk L.S <> stk L.K <> stk L.P]
  ,Entry "Page_Up" [stk R.P <> stk R.L]
  ,Entry "Page_Down" [stk R.B <> stk R.G]
  ,home
  ,end
  ,Entry "Delete" [stk R.Z]
  ,backspace]

controlKeys modifier control = map go
  where go (Entry n s) =
          Entry ("{}{#" ++ control n ++ "}{^}") $ map modifier s
