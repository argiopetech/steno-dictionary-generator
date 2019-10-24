module CSharp where

import Dictionary
import Keys
import Sounds
import Steno.Alphabet
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R

import Data.List (intersperse)
  

csharp =
  let else' = e <> l' <> s'
      if'   = i <> fvs'
  in entries
  [entry "{#control(z)}" [stk Hash <> stk Star]
  ,entry "{^};" [l <> e <> n' <> d']

  ,entry "{#control(k) control(s)}" [s <> r <> ow <> n' <> d']
  
  ,entry (ttl "if")                        [if']
  ,entry (tt "else")                       [else', else']
  ,entry "{#e l s e space i f Tab Tab}{>}" [else', if']
  ,entry (ttl "switch")                    [s <> w <> i <> ch']

  ,entry (ttl "for")     [f <> o <> r']
  ,entry (ttl "foreach") [ee <> ch']
  ,entry (ttl "do")      [d <> ew]
  ,entry (ttl "while")   [wh <> ii <> l']

  ,entry (ttu "class")     [k <> l <> a <> s']
  ,entry (ttu "struct")    [s <> t <> r <> u <> k' <> t']
  ,entry (ttu "interface") [n <> t' <> r', f <> aa <> s']
  ,entry (ttu "prop")      [p <> r <> o <> p']
  ,entry (ttu "indexer")   [n, d <> e <> x' <> r']
  ,entry (ttu "enum")      [ee, n <> u <> m']

  ,entry (ttu "try") [t <> r <> ii]
  
  ,entry "int{>}" [n <> t']
  ,entry "var{>}" [v <> aw <> r']
  ,entry "bool{>}" [b <> oo <> l']

  ,entry "&&" [n' <> d']
  ,entry "||" [o <> r']
  ,entry "!{^}"  [n <> o <> t']
  
  ,entry (tt "cw") [c, w]
  ,entry "Console." [c, p <> p']
  ,entry "Console.ReadLine()" [c, r]
  ]
  where tt w = ttr w ""
        ttl w = ttr w "{>}"
        ttu w = ttr w "{-|}"
        ttr w rest = "{#" ++ intersperse ' ' w ++ " Tab Tab}" ++ rest ++ "{^}"
