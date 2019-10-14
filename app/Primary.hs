{-# LANGUAGE RankNTypes #-}
module Primary where

import qualified Prelude as P
import Prelude ((<>), mconcat, (.), ($))

import Dictionary
import Stroke
import Suffixes
import Keys 
import qualified Keys.Left as L
import qualified Keys.Right as R


-- Left hand keys
s = stk L.S
t = stk L.T
k = stk L.K
p = stk L.P
w = stk L.W
h = stk L.H
r = stk L.R

-- left hand chorded sounds
b  = stks [L.P, L.W]
ch = stks [L.K, L.H]
d  = stks [L.T, L.K]
dh = stks [L.T, L.H] -- As in "the"
f  = stks [L.T, L.P]
g  = stks [L.T, L.K, L.P, L.W]
jh = stks [L.S, L.K, L.W, L.R]-- As in "jee"
l  = stks [L.H, L.R]
m  = stks [L.P, L.H]
n  = stks [L.T, L.P, L.H]
sh = stks [L.S, L.H]
th = dh -- As in "thing", "theta"
v  = stks [L.S, L.R]
wh = stks [L.W, L.H]
y  = stks [L.K, L.W, L.R]
z  = addModifier Star $ stk L.S

-- Right hand keys
fvs' = stk R.Fvs
r' = stk R.R
p' = stk R.P
b' = stk R.B
l' = stk R.L
g' = stk R.G
t' = stk R.T
s' = stk R.S
d' = stk R.D
z' = stk R.Z

-- Right hand chorded sounds
ch' = stks [R.Fvs, R.P]
dh' = stks [R.T]
k'  = stks [R.B, R.G]
m'  = stks [R.P, R.L]
n'  = stks [R.P, R.B]
ng' = n' <> stk R.G
sh' = stks [R.R, R.B]

-- Vowel sounds, still need some work
a = stks [A]           -- short 'a': bat
o = stks [A, U]        -- short 'o': bot
aw = o                 -- 'aw': ought, use 'o' instead
e = stks [E]           -- Short 'e': bed
i = stks [E, U]        -- short 'i': bit
u = stks [U]           -- short 'u': but

aa = stks [A, E, U]    -- long 'a': ate
ee = stks [A, E]       -- long 'e': eat
ii = stks [A, O, E, U] -- long 'i': hide
oe = stks [O, E]       -- long 'o': oat

ow = stks [O, U]       -- 'ow': cow
oi = stks [O, E, U]    -- 'oi': oil, toy
oo = stks [A, O]       -- 'oo': hood, book
ew = stks [O, U]       -- 'oo' for non-'oo' words: two


-- Word sounds
after = stks [a, fvs', t', r']
noon  = stks [n, oo, n']
dnt   = stks [d, n', t']
inter = stks [n, t', r']
it    = stks [i, t']
mail  = stks [m, aa, l']
stew  = stks [s, t, ew]
thank = stks [th, a, ng', k']
ty'    = stks [t', ee]
ry'    = stks [r', ee]


primaryDictionary = entries
  [entry "/" [stk O <> stk E <> stk U]
  ,entry "{^}{#Return}{^}{MODE:RESET}" [r <> r']
  ,entry "{^}{#Return}{^}{-|}" [w <> r <> r' <> b']
  ,entry "{^}{#Return}{#Return}{^}{-|}" [k <> w <> r <> r' <> b' <> g']
  ,entry "the"     [dh']
  ,entryS "Aaron"  [e <> r' <> n'] [contractS]
  ,entryS "Elliot" [e <> l' <> t'] [contractS]
  ,entryS "I"      [ii] [contractM]
  ,entry "a"       [aa]
  ,entry "after"   [after]
  ,entryS "afternoon" [after, noon] [plural]
  ,entry "all"   [aw <> l']
  ,entry "an"    [a <> n']
  ,entry "and"   [n' <> d']
  ,entry "are"   [a <> r']
  ,entry "as"    [a <> z']
  ,entry "at"    [a <> t']
  ,entry "be"    [b <> ee]
  ,entry "been"  [b <> i <> n']
  ,entry "best"  [b <> e <> fvs' <> t']
  ,entry "but"   [b <> u <> t']
  ,entry "by"    [b <> ii]
  ,entry "can"   [k <> a <> n']
  ,entryS "dent" [d <> e <> n' <> t'] [plural]
  ,entry "do"    [d <> ew]
  ,entryS "email" [ee, mail] [ing]
  ,entryS "faculty" [f <> a <> k' <> l' <> ty'] [contractS]
  ,entry "for"  [f <> o <> r']
  ,entry "from" [f <> r <> o <> m']
  ,entry "good" [g <> oo <> d']
  ,entry "have" [h <> a <> fvs']
  ,entry "if"   [i <> fvs']
  ,entry "in"   [n']
  ,entryS "interview" [inter, v <> ew] [plural]
  ,entry "is"   [i <> z']
  ,entryS "it"   [it] [plural, contractS]
  ,entryS "mail" [mail] [ing]
  ,entry "male" [rep [A, E] mail]
  ,entry "me"   [m <> ee]
  ,entry "more" [m <> oe <> r']
  ,entryS "morning" [m <> oe <> r' <> ng'] [plural, contractS]
  ,entry "my"  [m <> ii]
  ,entry "no"  [n <> oe]
  ,entry "noon" [noon]
  ,entry "not" [n <> o <> t']
  ,entry "of"  [u <> fvs']
  ,entryS "oil" [oi <> l'] [ing, ed]
  ,entry "on"  [o <> n']
  ,entry "or"  [o <> r']
  ,entry "ordinary" [o <> r' <> d', n <> e <> ry']
  ,entry "our" [ow <> r']
  ,entry "out" [ow <> t']
  ,entryS "present" [p <> r <> e <> z' <> n' <> t'] [ing, ed]
  ,entry "some"    [s <> u <> m']
  ,entry "stew"    [stew]
  ,entryS "student"  [stew, dnt] [plural, contractS, pluralPosessive]
  ,entryS "thank"  [thank] [plural, ing, ed]
  ,entry "that" [dh <> a <> t']
  ,entry "they" [dh <> aa]
  ,entry "this" [dh <> i <> s']
  ,entry "time" [t <> ii <> m']
  ,entry "to"   [t <> ew]
  ,entry "up"   [u <> p']
  ,entry "was"  [w <> u <> z']
  ,entry "we"   [w <> ee]
  ,entry "well" [w <> e <> l']
  ,entry "were" [w <> u <> r']
  ,entry "which" [wh <> i <> ch']
  ,entry "will"  [w <> i <> l']
  ,entry "with" [w <> i <> dh']
  ,entryS "wonder" [w <> u <> n', d' <> r'] [plural, ed, ing]
  ,entry "you"  [y <> ew]
  ,entry "your" [y <> o <> r']
  ]
