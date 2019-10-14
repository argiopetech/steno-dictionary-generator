{-# LANGUAGE RankNTypes #-}
module Primary where

import qualified Prelude as P
import Prelude ((<>), (.), ($))

import Dictionary
import Sounds
import Stroke
import Suffixes
import Keys 
import qualified Keys.Left as L
import qualified Keys.Right as R


-- Word sounds
after = stks [a, fvs', t', r']
noon  = stks [n, oo, n']
dnt   = stks [d, n', t']
inter = stks [n, t', r']

stew  = stks [s, t, ew]
thank = stks [th, a, ng', k']
ty'    = stks [t', ee]
ry'    = stks [r', ee]


primaryDictionary =
  let mail  = stks [m, aa, l']
  in entries
  [entry "{^}{#Return}{^}{MODE:RESET}" [r <> r']
  ,entry "{^}{#Return}{^}{-|}" [w <> r <> r' <> b']
  ,entry "{^}{#Return}{#Return}{^}{-|}" [k <> w <> r <> r' <> b' <> g']
  
  -- a section
  ,entry  "a"         [aa]
  ,entryS "Aaron"     [e <> r' <> n'] [contractS]
  ,entry  "after"     [after]
  ,entryS "afternoon" [after, noon] [plural]
  ,entry  "all"       [aw <> l']
  ,entry  "an"        [a <> n']
  ,entry  "and"       [n' <> d']
  ,entry  "are"       [a <> r']
  ,entry  "as"        [a <> z']
  ,entry  "at"        [a <> t']
  
  -- b section
  ,entry "be"     [b <> ee]
  ,entry "been"   [b <> i <> n']
  ,entry "best"   [b <> e <> fvs' <> t']
  ,entryS "break" [b <> r <> aa <> k'] [ing]
  ,entryS "build" [b <> i <> l' <> d'] [ing]
  ,entry "but"    [b <> u <> t']
  ,entry "by"     [b <> ii]
  
  -- c section
  ,entry "can"   [k <> a <> n']
  ,entry "cat"   [k <> a <> t']
  ,entry "cd"    [k <> r <> d'] -- as in, fingerspelling 'c'
  
  -- d section
  ,entryS "dent" [d <> e <> n' <> t'] [plural]
  ,entry "do"    [d <> ew]
  
  -- e section
  ,entryS "Elliot" [e <> l' <> t'] [contractS]
  ,entryS "email"  [ee, mail]      [ing]
  
  -- f section
  ,entryS "faculty" [f <> a <> k' <> l' <> ty'] [contractS]
  ,entry "for"      [f <> o <> r']
  ,entry "from"     [f <> r <> o <> m']
  
  -- g section
  ,entry "get"  [g <> e <> t']
  ,entry "git"  [g <> i <> t']
  ,entry "good" [g <> oo <> d']
  
  -- h section
  ,entry "have" [h <> a <> fvs']
  
  -- i section
  ,entryS "I"         [ii]             [contractM]
  ,entry "if"         [i <> fvs']
  ,entry "in"         [n']
  ,entryS "interview" [inter, v <> ew] [plural]
  ,entry "is"         [i <> z']
  ,entryS "it"        [i <> t']   [plural, contractS]
  
  -- j section
  
  -- k section
  ,entryS "kid"   [k <> i <> d']  [ing]
  ,entry  "kitty" [k <> i <> ty']
  
  -- l section
  ,entry  "less"  [l <> e <> s']
  ,entry  "ls"    [l <> s']
  
  -- m section
  ,entryS "mail"    [mail]
                    [ing]
  ,entry "male"    [rep [A, E] mail]
  ,entry "me"      [m <> ee]
  ,entry "more"    [m <> oe <> r']
  ,entryS "morning" [m <> oe <> r' <> ng']
                    [plural, contractS]
  ,entry  "my"      [m <> ii]
  
  -- n section
  ,entry "no"   [n <> oe]
  ,entry "noon" [noon]
  ,entry "not"  [n <> o <> t']
  
  -- o section
  ,entry  "of"       [u <> fvs']
  ,entryS "oil"      [oi <> l']
                     [ing, ed]
  ,entry  "on"       [o <> n']
  ,entry  "or"       [o <> r']
  ,entry  "ordinary" [o <> r' <> d', n <> e <> ry']
  ,entry  "our"      [ow <> r']
  ,entry  "out"      [ow <> t']
  
  -- p section
  ,entryS "present" [p <> r <> e <> z' <> n' <> t']
                    [ing, ed]
  
  -- q section
  
  -- r section
  
  -- s section
  ,entry  "some"    [s <> u <> m']
  ,entry  "stew"    [stew]
  ,entryS "student" [stew, dnt]
                    [plural, contractS, pluralPosessive]

  -- t section
  ,entryS "thank" [thank]
                  [plural, ing, ed]
  ,entry  "that"  [dh <> a <> t']
  ,entry  "the"   [dh']
  ,entry  "they"  [dh <> aa]
  ,entry  "this"  [dh <> i <> s']
  ,entryS "time"  [t <> ii <> m']
                  [plural]
  ,entry  "to"    [t <> ew]
  
  -- u section
  ,entry  "up"   [u <> p']
  
  -- v section
  
  -- w section
  ,entry  "was"    [w <> u <> z']
  ,entry  "we"     [w <> ee]
  ,entry  "well"   [w <> e <> l']
  ,entry  "were"   [w <> u <> r']
  ,entry  "which"  [wh <> i <> ch']
  ,entry  "will"   [w <> i <> l']
  ,entry  "with"   [w <> i <> dh']
  ,entryS "wonder" [w <> u <> n', d' <> r']
                   [plural, ed, ing]
  
  -- x section
  
  -- y section
  ,entry  "you"  [y <> ew]
  ,entry  "your" [y <> o <> r']
  
  -- z section
  ,entryS "zap" [z <> a <> p'] [ed]
  ]
