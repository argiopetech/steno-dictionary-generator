module XMonad where

import Dictionary
import Keys
import Sounds
import Steno.Numbers
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R

import Text.Printf


switchDesktopsLeftHand = stks [s <> w]
switchDesktopsRightHand = stks [b' <> s']
moveWindowLeftHand = stks [s <> w <> stk Star]
moveWindowRightHand = stks [b' <> s' <> star]

leftHand = (switchDesktopsLeftHand, moveWindowLeftHand)
rightHand = (switchDesktopsRightHand, moveWindowRightHand)


-- This can be a single stroke with SW for 0,5-9 and -BS for 1-4
-- xmonad = entries $
--   [entry "{#}" [switchDesktops]
--   ,entry "{#}" [moveWindow]] ++ map go numbers
--   where go (Entry n s) =
--           [Entry (printf "{#super(%s)}{^}" n) (switchDesktops : s)
--           ,Entry (printf "{#super(shift(%s))}{^}" n) (moveWindow : s)]

xmonad = entries $ (map (go rightHand) $ take 6 numbers) ++ (map (go leftHand) $ drop 6 numbers)
  where go (switch, move) (Entry n s) =
          [Entry (printf "{#super(%s)}{^}" n) (map (switch <>) s)
          ,Entry (printf "{#super(shift(%s))}{^}" n) (map (move <>) s)]
