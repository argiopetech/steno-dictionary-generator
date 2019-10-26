module Sounds where

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
c  = stks [L.K, L.R]
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
wr = stks [L.W, L.R]
x  = stks [L.K, L.P]
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
dh' = stks [R.T]
k'  = stks [R.B, R.G]
m'  = stks [R.P, R.L]
n'  = stks [R.P, R.B]
ng' = n' <> stk R.G
sh' = stks [fvs', p']
ch' = stks [r', b']
shn' = stks [sh', n']
th'  = stks [t']
x' = stks [k', s']

-- Vowel sounds, still need some work
a = stks [A]           -- short 'a': bat
o = stks [A, U]        -- short 'o': bot
aw = o                 -- 'aw': ought, use 'o' instead
e = stks [E]           -- Short 'e': bed
i = stks [E, U]        -- short 'i': bit
u = stks [U]           -- short 'u': but

aa = stks [A, E, U]    -- long 'a': ate
ee = stks [A, E]       -- long 'e': eat, originally AE
ii = stks [A, O, E, U] -- long 'i': hide
oe = stks [O, E]       -- long 'o': oat

ow = stks [A, O, U]    -- 'ow': cow
oi = stks [O, E, U]    -- 'oi': oil, toy
oo = stks [A, O]       -- 'oo': hood, book
ew = stks [O, U]       -- 'oo' for non-'oo' words: two, originally OU

er = stk U <> stk R.R
