{-# LANGUAGE RankNTypes #-}
module Primary where

import qualified Prelude as P
import Prelude ((<>), mconcat, (.), map)

import Dictionary
import Stroke
import Keys 
import qualified Keys.Left as L
import qualified Keys.Right as R

-- left hand chorded sounds
bl  = stks [L.P, L.W]
chl = stks [L.K, L.H]
dl  = stks [L.T, L.K]
dhl = stks [L.T, L.H] -- As in "the"
fl  = stks [L.T, L.P]
gl  = stks [L.T, L.K, L.P, L.W]
jhl = stks [L.S, L.K, L.W, L.R]-- As in "jee"
ll  = stks [L.H, L.R]
ml  = stks [L.P, L.H]
nl  = stks [L.T, L.P, L.H]
shl = stks [L.S, L.H]
thl = dhl -- As in "thing", "theta"
vl  = stks [L.S, L.R]
yl  = stks [L.K, L.W, L.R]
zl  = stks [L.S] -- Not happy with this one

-- Right hand chorded sounds
chr = stks [R.Fvs, R.P]
dhr = stks [R.T]
kr  = stks [R.B, R.G]
nr  = stks [R.P, R.B]
ngr = nr <> stks [R.G]
shr = stks [R.R, R.B]

-- Vowel sounds, still need some work
aa = stks [A, U]       -- short 'o': odd
ae = stks [A]          -- short 'a': at
ah = stks [U]          -- short 'u': hut, of
ao = stks [A, U]       -- 'aw': ought
aw = stks [O, U]       -- 'ow': cow
ay = stks [A, O, E, U] -- long 'i': hide
eh = stks [E]          -- Short 'e': Ed
er = stks [E]          -- 'er' pair... Not sure I like this one
ey = stks [A, E, U]    -- long 'a': ate
ih = stks [E, U]       -- short 'i': it
iy = stks [A, E]       -- long 'e': eat
ow = stks [O, E]       -- long 'o': oat
oy = stks [O, E, U]    -- 'oi': oil, toy
uh = stks [A, O]       -- 'oo': hood, book
uw = stks [U]          -- 'oo' for non-'oo' words: two


-- Word sounds
you  = stks [yl, uw]
to   = stks [stk L.T, uw]
for  = stks [fl, ao, stk R.R]
of'  = stks [ah, stk R.Fvs]
that = stks [dhl, ae, stk R.T]
this = stks [dhl, ih, stk R.S]
is   = stks [ih, stk R.Z]
on   = stks [aa, nr]
as   = stks [ae, stk R.Z]
with = stks [stk L.W, ih, dhr]
be   = stks [bl, iy]
your = stks [yl, ao, stk R.R]
have = stks [stk L.H, ae, stk R.Fvs]
at   = stks [ae, stk R.T]
if'  = stks [ih, stk R.Fvs]
good = stks [gl, uh, stk R.D]
all  = stks [ao, stk R.L]
we   = stks [stk L.W, iy]
thank = stks [thl, ae, ngr, kr]
oil   = stks [oy, stk R.L]

stew = stks [stks [L.S, L.T], uw]
dnt  = stks [dl, nr, stk R.T]
facl = stks [fl, ae, kr, stk R.L]
ty   = stks [iy, stk R.T]


primaryDictionary =
  [Entry "the"  [dhl]
  ,Entry "to"   [to]
  ,Entry "I"    [ay]
  ,Entry "for"  [for]
  ,Entry "and"  [nr <> stk R.D]
  ,Entry "a"    [ey]
  ,Entry "in"   [nr]
  ,Entry "of"   [of']
  ,Entry "that" [that]
  ,Entry "this" [this]
  ,Entry "you"  [you]
  ,Entry "is"   [is]
  ,Entry "on"   [on]
  ,Entry "as"   [as]
  ,Entry "with" [with]
  ,Entry "be"   [be]
  ,Entry "your" [your]
  ,Entry "have" [have]
  ,Entry "at"   [at]
  ,Entry "if"   [if']
  ,Entry "good" [good]
  ,Entry "all"  [all]
  ,Entry "we"   [we]
  ,Entry "thank" [thank]
  ,Entry "thanks" [thank <> stk R.S]
  ,Entry "stew" [stew]
  ,Entry "dent" [dnt <> eh]
  ,Entry "student"  [stew, dnt]
  ,Entry "students" [stew, dnt <> stk R.S]
  ,Entry "was"      [stk L.W, stk U, stk R.Z]
  ,Entry "not"      []
  ,Entry "it"       []
  ,Entry "Elliot"   [eh <> stks [R.L, R.T]]
  ,Entry "I'm"      []
  ,Entry "or"       []
  ,Entry "faculty"  [facl <> ty]
  ,Entry "my"    []
  ,Entry "from"  []
  ,Entry "email" []
  ,Entry "some"  []
  ,Entry "can"   []
  ,Entry "but"   []
  ,Entry "best"  []
  ,Entry "are"   []
  ,Entry "afternoon" []
  ,Entry "Aaron"     []
  ,Entry "time"      []
  ,Entry "they"      []
  ,Entry "present" []
  ,Entry "morning" []
  ,Entry "by"    []
  ,Entry "an"    []
  ,Entry "will"  []
  ,Entry "which" []
  ,Entry "out" []
  ,Entry "our" []
  ,Entry "me"  []
  ,Entry "do"  []
  ,Entry "interviews" []
  ,Entry "been" []
  ,Entry "were" []
  ,Entry "well" []
  ,Entry "up" []
  ,Entry "no" []
  ,Entry "more" []
  ,Entry "it's" []
  ,Entry "its" []
  ]
