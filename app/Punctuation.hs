module Punctuation where

import Dictionary
import Keys
import Steno.Alphabet
import Stroke
import Sounds

import qualified Keys.Left as L
import qualified Keys.Right as R

dot = Entry "." [d <> o <> t']

coding = entries
  [[dot]
  ,entry "wc" [w, c]
  ,entry "mv" [m <> fvs']
  ,entry "rm" [r <> m']
  ,entry "cp" [c <> p']
  ]


punctuation =
  let equal = k <> w <> l
      shun = g' <> s'
      ren = r <> e <> n'

     -- Basic punctuation
  in [Entry "{,}" [stk L.K <> stk L.W <> stk R.B <> stk R.G]
     ,Entry "{.}" [stk L.T <> stk L.P <> stk R.P <> stk R.L]

     ,Entry "{!}" [stk L.K <> stk L.W <> stk R.P <> stk R.L]
     ,Entry "{?}" [stk L.T <> stk L.P <> stk R.B <> stk R.G]

     -- Attached punctuation, for code and .com & etc.
     ,Entry "{^,^}" [w <> b']
     ,Entry "{^.^}" [p <> p']


     -- Surrounding characters
     -- Quotes
     -- "kwotashun" - w opens, r closes. Comes from plover
     ,Entry "{~|\\\"^}" [k <> w <> shun] -- Kwotes
     ,Entry "{^~|\\\"}" [k <> r <> shun]
     ,Entry "{~|\'^}"   [s <> w <> shun] -- Single quotes
     ,Entry "{^~|\'}"   [s <> r <> shun]
     ,Entry "{~|`^}"    [b <> shun]      -- Backticks
     ,Entry "{^~|`}"    [b <> r <> shun]

     -- Brackets
     ,Entry "{~|(^}"     [p <> ren]            -- Parens
     ,Entry "{^~|)}"     [p <> ren <> stk R.T]
     ,Entry "{~|\\\\{^}" [k <> ren]            -- Kurly brackets
     ,Entry "{^~|\\\\}}" [k <> ren <> stk R.T]
     ,Entry "{~|[^}"     [s <> ren]            -- Square brackets
     ,Entry "{^~|]}"     [s <> ren <> stk R.T]
     ,Entry "<{^}"       [w <> a <> k']        -- [[http://www.tnellen.com/cybereng/asciipoemcodes][waka waka]]
     ,Entry "{^}>"       [w <> a <> k' <> t']

     -- Horizontal lines
     ,Entry "{^-^}"  [h <> r']       -- Horizontal rule
     ,Entry "{^--^}" [h <> r' <> b'] -- HR + a right hand finger to widen it
     ,Entry "{^_^}"  [s <> k <> r']

     ,Entry "="      [equal]
     ,Entry "=="     [equal, equal]

     ,Entry "~"      [t <> l <> d']
     ,Entry "{^}#"   [h <> a <> sh' <> star]

     -- The whole top row
     ,Entry ":"      [s <> t <> p <> h <> fvs' <> p' <> l' <> t']
     ,Entry "::"     [s <> t <> p <> h <> fvs' <> p' <> l' <> t', s <> t <> p <> h <> fvs' <> p' <> l' <> t']
     ,Entry ";"      [s <> t <> p <> h <> fvs' <> p' <> l'] -- Minus t'
     ,Entry ";;"      [s <> t <> p <> h <> fvs' <> p' <> l', s <> t <> p <> h <> fvs' <> p' <> l']

     ,Entry "$"       [d' <> hash]
     ,Entry "%"       [p <> fvs' <> t']
     ,Entry "{^}@{^}" [a <> t' <> star]

     ,Entry "&"      [m <> p']
     ,Entry "&&"     [m <> p', m <> p']

     ,Entry "*"      [s <> t <> r]
     ,Entry "**"     [s <> t <> r, s <> t <> r]

     ,Entry "|"      [p <> ii <> p' <> star]
     ,Entry "||"     [p <> ii <> p' <> star, p <> ii <> p' <> star]

     ,Entry "^"      [u <> p' <> star]
     ,Entry "^^"     [u <> p' <> star, u <> p' <> star]

     ,Entry "{^/^}"    [stk O <> e <> u]
     ,Entry "{^\\\\^}" [s <> l <> a]
     ]
