module Steno.Alphabet where

import Dictionary
import Stroke
import Keys 
import qualified Keys.Left as L

import Data.Char (toUpper)


la = Entry "a" [vowels [A]]
lb = Entry "b" [lefts [L.P, L.W]]
lc = Entry "c" [lefts [L.K, L.R]]
ld = Entry "d" [lefts [L.T, L.K]]
le = Entry "e" [vowels [E]]
lf = Entry "f" [lefts [L.T, L.P]]
lg = Entry "g" [lefts [L.T, L.K, L.P, L.W]]
lh = Entry "h" [lefts [L.H]]
li = Entry "i" [vowels [E, U]]
lj = Entry "j" [lefts [L.S, L.K, L.W, L.R]]
lk = Entry "k" [lefts [L.K]]
ll = Entry "l" [lefts [L.H, L.R]]
lm = Entry "m" [lefts [L.P, L.H]]
ln = Entry "n" [lefts [L.T, L.P, L.H]]
lo = Entry "o" [vowels [O]]
lp = Entry "p" [lefts [L.P]]
lq = Entry "q" [lefts [L.K, L.W]]
lr = Entry "r" [lefts [L.R]]
ls = Entry "s" [lefts [L.S]]
lt = Entry "t" [lefts [L.T]]
lu = Entry "u" [vowels [U]]
lv = Entry "v" [lefts [L.S, L.R]]
lw = Entry "w" [lefts [L.W]]
lx = Entry "x" [lefts [L.K, L.P]]
ly = Entry "y" [lefts [L.K, L.W, L.R]]
lz = Entry "z" [lefts [L.S, L.T, L.K, L.P, L.W]]

alphabet = [la, lb, lc, ld, le, lf, lg, lh, li, lj, lk, ll, lm
           ,ln, lo, lp, lq, lr, ls, lt, lu, lv, lw, lx, ly, lz]


fingerspelling modifier capsMod = concatMap go alphabet
  where go (Entry n s) =
          [Entry ("{>}{&" ++ n ++ "}") $ map modifier s
          ,Entry ("{&" ++ map toUpper n ++ "}") $ map (modifier . capsMod) s]
