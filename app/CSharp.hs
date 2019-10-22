module CSharp where

import Dictionary
import Keys
import Sounds
import Steno.Alphabet
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R

csharp = entries
  [entry "{#c w Tab Tab}" [c, w]
  ,entry "Console." [c, p <> p']
  ,entry "Console.ReadLine()" [c, r]
  ,entry "{#control(z)}" [stk Hash <> stk Star]
  ,entry "{^};" [l <> e <> n' <> d']
  ]
