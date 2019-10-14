module ControlKeys where

import Dictionary
import Keys
import Steno.Alphabet
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R

arrows = 
  [Entry "Left" [stk R.Fvs]
  ,Entry "Right" [stk R.T]
  ,Entry "Up" [stk R.L]
  ,Entry "Down" [stk R.P]]

home = Entry "Home" [stk R.P <> stk R.B]
space = Entry "Space" [stk L.S <> stk L.P]
enter = Entry "Return" [stk L.R <> stk L.T]

specialKeys =
  [Entry "Tab" [stk L.T <> stk A]
  ,Entry "Escape" [stk L.S <> stk L.K <> stk L.P]
  ,Entry "PgUp" [stk R.P <> stk R.L]
  ,Entry "PgDown" [stk R.B <> stk R.G]
  ,home
  ,Entry "End" [stk R.T <> stk R.S]]

controlKeys modifier control = map go
  where go (Entry n s) =
          Entry ("{}{#" ++ control n ++ "}") $ map modifier s
