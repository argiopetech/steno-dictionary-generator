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


primaryDictionary =
   -- Word sounds
  let mail  = stks [m, aa, l']
      after = stks [a, fvs', t', r']
      come  = stks [k, o, m']
      noon  = stks [n, oo, n']
      dnt   = stks [d, n', t']
      inter = stks [n, t', r']
      stew  = stks [s, t, ew]
      sure  = stks [s, oo, r']
      thank = stks [th, a, ng', k']
      well  = stks [w, e, l']

      ty'    = stks [t', ee]
      ry'    = stks [r', ee]
      ly'    = stks [l', ee]
      nd'    = stks [n', d']
  in entries
  [entry "{^}{#Return}{^}{MODE:RESET}" [r <> r' <> b']
  ,entry "{^}\\n{^}{-|}" [r <> r']
  ,entry "{^}\\n\\n{^}{-|}" [w <> r <> r' <> b']

  -- a section
  ,entry  "a"         [aa]
  ,entryS "Aaron"     [e <> r' <> n'] [contractS]
  ,entry  "add"       [a <> d']
  ,entry  "after"     [after]
  ,entryS "afternoon" [after, noon] [plural]
  ,entry  "all"       [aw <> l']
  ,entry  "an"        [a <> n']
  ,entry  "and"       [nd']
  ,entry  "are"       [a <> r']
  ,entry  "as"        [a <> z']
  ,entry  "at"        [a <> t']
  ,entry  "available" [v <> aa <> b' <> l']

  -- b section
  ,entry  "be"    [b <> ee]
  ,entry  "been"  [b <> i <> n']
  ,entry  "best"  [b <> e <> fvs' <> t']
  ,entryS "break" [b <> r <> aa <> k'] [ing]
  ,entry  "broadcast" [b <> r <> o <> d', k <> a <> fvs' <> t']
  ,entryS "build" [b <> i <> l' <> d'] [ing]
  ,entry  "but"   [b <> u <> t']
  ,entry  "by"    [b <> ii]

  -- c section
  ,entry  "can"    [k <> a <> n']
  ,entry  "cat"    [k <> a <> t']
  ,entry  "cd"     [k <> r <> d'] -- as in, fingerspelling 'c'
  ,entry  "chrome" [k <> r <> oe <> m']
  ,entryS "come"   [come]
                   [ing]
  ,entry  "commit" [k <> m <> i <> t']

  -- d section
  ,entry  "date" [d <> aa <> t']
  ,entryS "dent" [d <> e <> n' <> t'] [plural]
  ,entry  "diff" [d <> i <> fvs']
  ,entry  "do"   [d <> ew]

  -- e section
  ,entry  "early"     [r <> l]
  ,entry  "echo"      [e <> k']
  ,entryS "Elliot"    [e <> l' <> t'] [contractS]
  ,entryS "email"     [ee, mail]      [ing]
  ,entry  "emergency" [m <> e <> r' <> g', n <> s' <> ee]
  ,entry  "Emily"     [e <> m, ly']
  ,entry  "entry"     [n, t <> r <> ee]
   
  -- f section
  ,entryS "faculty" [f <> a <> k' <> l' <> ty']
                    [contractS]
  ,entry  "find"    [f <> ii <> nd']
  ,entry  "for"     [f <> o <> r']
  ,entry  "free"    [f <> r <> ee]
  ,entry  "from"    [f <> r <> o <> m']

  -- g section
  ,entry "get"   [g <> e <> t']
  ,entry "git"   [g <> i <> t']
  ,entry "good"  [g <> oo <> d']
  ,entry "great" [g <> r <> aa <> t']

  -- h section
  ,entryS "have" [h <> a <> fvs']
                 [ing]
  ,entry  "how"  [h <> ow]

  -- i section
  ,entryS "I"         [ii]             [contractM, contractD]
  ,entry  "if"        [i <> fvs']
  ,entry  "in"        [n']
  ,entryS "interview" [inter, v <> ew] [plural]
  ,entry  "into"      [n <> t']
  ,entry  "is"        [i <> z']
  ,entryS "it"        [i <> t']        [plural, contractS]

  -- j section

  -- k section
  ,entryS "kid"   [k <> i <> d']  [ing]
  ,entry  "kitty" [k <> i <> ty']
  ,entry  "know"  [k <> n <> oe]

  -- l section
  ,entry  "less"  [l <> e <> s']
  ,entry  "let"   [l <> e <> t']
  ,entryS "look"  [l <> oo <> k']
                  [ing]
  ,entry  "like"  [l <> ii <> k']
  ,entry  "ls"    [l <> s']

  -- m section
  ,entryS "mail"    [mail]
                    [ing]
  ,entry "male"    [rep [A, E] mail]
  ,entry "may"     [m <> aa]
  ,entry "me"      [m <> ee]
  ,entry "more"    [m <> oe <> r']
  ,entryS "morning" [m <> oe <> r' <> ng']
                    [plural, contractS]
  ,entry  "my"      [m <> ii]

  -- n section
  ,entry "no"   [n <> oe]
  ,entry "noon" [noon]
  ,entry "not"  [n <> o <> t']
  ,entry "now"  [n <> ow]

  -- o section
  ,entry  "of"       [u <> fvs']
  ,entry  "occur"    [k <> u <> r']
  ,entryS "oil"      [oi <> l']
                     [ing, ed]
  ,entry  "on"       [o <> n']
  ,entry  "only"     [oe <> n' <> ly']
  ,entry  "or"       [o <> r']
  ,entryS "ordinary" [o <> r' <> d', n <> e <> ry']
                     [ly]
  ,entry  "our"      [ow <> r']
  ,entry  "out"      [ow <> t']

  -- p section
  ,entryS "present" [p <> r <> e <> z' <> n' <> t']
                    [ing, ed, ly]
  ,entry  "push"    [p <> oo <> sh']

  -- q section

  -- r section
  ,entryS "reason"  [r <> ee <> fvs' <> n']
                    [plural]
  ,entry  "real"    [r <> ee <> l']
  ,entry  "right"   [r <> ii <> t']
  ,entry  "run"     [r <> u <> n']

  -- s section
  ,entry  "says"       [s <> e <> z']
  ,entry  "so"         [s <> oe]
  ,entry  "some"       [s <> u <> m']
  ,entryS "sound"      [s <> ow <> nd']
                       [plural]
  ,entry  "stack"      [s <> t <> a <> k']
  ,entry  "status"     [s <> t <> a <> t' <> s']
  ,entry  "statistics" [s <> t <> a <> t' <> z']
  ,entry  "stew"       [stew]
  ,entryS "student"    [stew, dnt]
                       [plural, contractS, pluralPosessive]
  ,entryS "sure"       [sure]
                       [ly]
  ,entry  "system"     [s <> i <> s', t <> m']

  -- t section
  ,entry  "test"  [t <> e <> fvs' <> t']
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
  ,entry  "use"  [y <> ew <> s']

  -- v section
  ,entry  "version" [v <> e <> r' <> shn']

  -- w section
  ,entry  "was"     [w <> u <> z']
  ,entry  "we"      [w <> ee]
  ,entry  "well"    [well]
  ,entryS "welcome" [well, come]
                    [ing]
  ,entry  "were"    [w <> u <> r']
  ,entry  "what"    [w <> u <> t']
  ,entry  "which"   [wh <> i <> ch']
  ,entry  "will"    [w <> i <> l']
  ,entry  "with"    [w <> i <> dh']
  ,entryS "wonder"  [w <> u <> n', d' <> r']
                    [plural, ed, ing]
  ,entry  "would"   [w <> oo <> d']
  ,entry  "world"   [w <> u <> r' <> l' <> d']

  -- x section

  -- y section
  ,entry  "you"  [y <> ew]
  ,entry  "your" [y <> o <> r']

  -- z section
  ,entryS "zap" [z <> a <> p'] [ed]
  ]
