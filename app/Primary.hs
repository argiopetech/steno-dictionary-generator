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
  let mail = stks [m, aa, l']
      noon = stks [n, oo, n']
      write = stks [wr, ii, t']
      see   = stks [s', ee]
      till  = t <> i <> l'
      you   = y <> ew

      -- Suffixes
      dnt  = stks [d, n', t']
      ty'  = stks [t', ee]
      ry'  = stks [r', ee]
      ly'  = stks [l', ee]
      nd'  = stks [n', d']
  in entries
  [entry "{^}{#Return}{^}{MODE:RESET}" [r <> r' <> b']
  ,entry "{^}\\n{^}{-|}" [r <> r']
  ,entry "{^}\\n\\n{^}{-|}" [w <> r <> r' <> b']] <>

  -- a section
  let after = stks [a, fvs', t', r']
      agree = g <> r <> ee
  in entries
  [entry  "a"         [aa]
  ,entryS "Aaron"     [e <> r' <> n']
                      [contractS]
  ,entry  "able"      [aa <> b' <> l']
  ,entry  "about"     [b <> ow <> t']
  ,entry  "ace"       [aa <> s']
  ,entry  "actually"  [a <> k', ch <> ew <> ly']
  ,entry  "add"       [a <> d']
  ,entry  "after"     [after]
  ,entryS "afternoon" [after, noon]
                      [plural]
  ,entry  "against"   [g <> e <> n' <> s' <> t']
  ,entry  "agree"     [agree]
  ,entry  "agreement" [agree, m <> n' <> t']
  ,entry  "all"       [aw <> l']
  ,entry  "allowance" [l <> ow <> n' <> s']
  ,entry  "almost"    [aw <> l', m <> oe <> fvs' <> t']
  ,entry  "among"     [m <> u <> ng']
  ,entry  "amount"    [m, ow <> n' <> t'] -- m',?
  ,entry  "an"        [a <> n']
  ,entry  "and"       [nd']
  ,entry  "anger"     [g <> r <> r']
  ,entry  "announce"  [n, ow <> n' <> s'] -- n'?
  ,entry  "another"   [n <> u <> dh' <> r']
  ,entry  "any"       [n <> ee]
  ,entryS "apparent"  [p <> e <> r' <> n' <> t'] [ly]
  ,entry  "anything"  [e <> n' <> ee, th <> i <> ng']
  ,entry  "ape"       [aa <> p']
  ,entry  "April"     [aa, p <> r' <> l']
  ,entry  "are"       [a <> r']
  ,entry  "arm"       [a <> r' <> m']
  ,entry  "as"        [a <> z']
  ,entry  "ash"       [a <> sh']
  ,entry  "ask"       [a <> fvs' <> k']
  ,entry  "at"        [a <> t']
  ,entry  "available" [v <> aa <> l', b' <> l']
--  ,entry  "available" [v <> aa <> b' <> l'] -- Potential brief
  ,entryS "average"   [a <> fvs' <> r' <> g']
                      [plural]
  ,entry  "awful"     [aw <> fvs' <> l']
  ,entry  "Azure"     [a <> z', r']] <>

  -- b section
  let br = b <> r
  in entries
  [entry  "balm"      [b <> aw <> m']
  ,entry  "balmy"     [b <> aw <> m' <> ee]
  ,entry  "be"        [b <> ee]
  ,entry  "became"    [b, k <> aa <> m']
  ,entry  "because"   [b, k <> aw <> z']
  ,entryS "become"    [b, k <> aw <> m'] [plural]
  ,entry  "been"      [b <> i <> n']
  ,entry  "before"    [b, f <> oe <> r']
  ,entryS "begin"     [b, g <> i <> n'] [ing]
  ,entry  "beige"     [b <> aa <> zh']
  ,entry  "being"     [b <> ee <> ng']
  ,entry  "belong"    [b, l <> o <> ng']
  ,entry  "bento"     [b <> i <> n', t <> oe]
  ,entry  "best"      [b <> e <> fvs' <> t']
  ,entry  "bimodal"   [b <> ii, m <> oe, d <> l']
  ,entry  "birth"     [b <> u <> r' <> th']
  ,entryS "bin"       [b <> i, n']
                      [ed]
  ,entry  "blame"     [b <> l <> aa <> m']
  ,entry  "bleach"    [b <> l <> ee <> ch']
  ,entry  "blithe"    [b <> l <> ii <> dh']
  ,entry  "blush"     [b <> l <> u <> sh']
  ,entry  "board"     [b <> o <> r', d']
  ,entry  "bomb"      [b <> aw <> m', b']
  ,entry  "bored"     [b <> o <> r' <> d']
  ,entry  "botch"     [b <> o <> ch']
  ,entry  "bottom"    [b <> o <> t' <> m']
  ,entryS "break"     [br <> aa <> k'] [ing]
  ,entry  "breath"    [br <> ee <> th']
  ,entry  "breath"    [br <> e <> th']
  ,entry  "bring"     [br <> g']
  ,entry  "broadcast" [br <> o <> d', k <> a <> fvs' <> t']
  ,entry  "bruise"    [br <> ew <> s']
  ,entry  "brush"     [br <> u <> sh']
  ,entry  "brutal"    [br <> ew <> t' <> l']
  ,entryS "budget"    [b <> u <> j' <> t'] [plural]
  ,entryS "build"     [b <> i <> l' <> d'] [ing]
  ,entry  "bush"      [b <> oo <> sh]
  ,entry  "but"       [b <> u <> t']
  ,entry  "buyer"     [b <> ii <> r']
  ,entry  "by"        [b <> ii]] <>

  -- c section
  let car   = stks [k, aw, r']
      cert  = s <> er <> t'
      come  = stks [k, o, m']
      con   = k <> o <> n'
  in entries
  [entry  "cab"       [k <> a <> b']
  ,entry  "cad"       [k <> a <> d']
  ,entry  "call"      [k <> aw <> l']
  ,entryS "calm"      [k <> aw, m']
                      [ed]
  ,entry  "came"      [k <> aa <> m']
  ,entry  "can"       [k <> a <> n']
  ,entry  "car"       [car]
  ,entry  "carriage"  [k <> a <> r' <> j']
  ,entry  "Carly"     [car, ly']
  ,entry  "castle"    [k <> a <> fvs' <> l']
  ,entry  "cat"       [k <> a <> t']
  ,entry  "catch"     [k <> a <> ch']
  ,entry  "caught"    [k <> aw <> t']
  ,entry  "cd"        [k <> r <> d'] -- as in, fingerspelling 'c'
  ,entry  "cease"     [s <> ee <> s']
  ,entry  "cement"    [s <> m <> e <> n' <> t']
  ,entry  "cert"      [cert]
  ,entry  "certification" [cert, f <> k', aa <> sh' <> n']
  ,entry  "chaff"     [ch <> a <> fvs']
  ,entry  "chance"    [ch <> a <> n' <> s']
  ,entry  "change"    [ch <> aa <> n' <> g']
  ,entry  "channel"   [ch <> a <> n' <> l']
  ,entry  "chant"     [ch <> a <> n' <> t']
  ,entry  "charm"     [ch <> a <> r' <> m']
  ,entry  "chart"     [ch <> a <> r' <> t']
  ,entry  "chasm"     [k <> a <> fvs' <> m']
  ,entry  "chat"      [ch <> a <> t']
  ,entry  "check"     [ch <> e <> k']
  ,entry  "chess"     [ch <> e <> s']
  ,entry  "chest"     [ch <> e <> s' <> t']
  ,entry  "chin"      [ch <> i <> n']
  ,entry  "chive"     [ch <> ii <> fvs']
  ,entry  "choose"    [ch <> oo <> s']
  ,entry  "chop"      [ch <> o <> p']
  ,entry  "chore"     [ch <> oe <> r']
  ,entry  "chrome"    [k <> r <> oe <> m']
  ,entry  "chuck"     [ch <> u <> k']
  ,entry  "churn"     [ch <> u <> r' <> n']
  ,entry  "coat"      [k <> oe <> t']
  ,entry  "cod"       [k <> o <> d']
  ,entry  "code"      [k <> oe <> d']
  ,entry  "cog"       [k <> o <> g']
  ,entry  "coil"      [k <> oi <> l']
  ,entryS "come"      [come]
                      [ing]
  ,entry  "commend"   [k <> m <> e <> n' <> d']
  ,entry  "commit"    [k <> m <> i <> t']
  ,entry  "con"       [con]
  ,entryS "conflict"  [con, f <> l <> i <> k' <> t']
                      [ed, ing]
  ,entry  "contract"  [con, t <> r <> a <> k' <> t']
  ,entryS "copy"      [k <> aw <> p' <> ee]
                      [plural]
  ,entry  "crystal"   [k <> r <> i <> fvs' <> t' <> l']] <>

  -- d section
  let day = d <> aa
  in entries
  [entryS "date"   [day <> t']
                   [plural]
  ,entryS "day"    [day]
                   [plural]
  ,entryS "dent"   [d <> e <> n' <> t']
                   [plural]
  ,entry  "diff"   [d <> i <> fvs']
  ,entry  "do"     [d <> ew]
  ,entry  "don't"  [d <> oe <> n' <> t']
  ,entry  "does"   [d <> u <> z']
  ,entry  "doesn't" [d <> u <> z', n' <> t']
  ,entry  "done"   [d <> u <> n']
  ,entryS "double" [d <> u <> b' <> l']
                   [ing, ed, plural]] <>

  -- e section
  entries
  [entry  "early"     [er <> l']
  ,entry  "echo"      [e <> k']
  ,entryS "effort"    [e, f <> r' <> t']
                      [plural]
  ,entryS "Elliot"    [e <> l' <> t']
                      [contractS]
  ,entry  "else"      [e <> l' <> s']
  ,entryS "email"     [ee, mail]
                      [ing]
  ,entry  "emacs"     [ee, m <> a <> x']
  ,entry  "emergency" [m <> er <> g', n <> s' <> ee]
  ,entry  "Emily"     [e <> m, ly']
  ,entryS "entry"     [n, t <> r <> ee] [plural]
  ,entry  "except"    [x <> e <> p' <> t']] <>

  -- f section
  entries
  [entryS "faculty" [f <> a <> k' <> l' <> ty']
                    [contractS]
  ,entry  "false"   [f <> aw <> l' <> s']
  ,entry  "find"    [f <> ii <> nd']
  ,entry  "fire"    [f <> ii <> r']
  ,entry  "for"     [f <> o <> r']
  ,entry  "float"   [f <> l <> oe <> t']
  ,entry  "free"    [f <> r <> ee]
  ,entry  "from"    [f <> r <> o <> m']] <>

  -- g section
  entries
  [entry  "get"       [g <> e <> t']
  ,entry  "gift"      [g <> i <> fvs' <> t']
  ,entry  "git"       [g <> i <> t']
  ,entry  "give"      [g <> i <> fvs']
  ,entry  "given"     [g <> i <> fvs' <> n']
  ,entry  "good"      [g <> oo <> d']
  ,entry  "great"     [g <> r <> aa <> t']
  ,entryS "guarantee" [g <> e <> r' <> n' <> ty']
                      [ed, plural]] <>

  -- h section
  let hand = stks [h <> a <> n' <> d']
  in entries
  [entry  "had"         [h <> a <> d']
  ,entry  "has"         [h <> a <> z']
  ,entry  "hand"        [hand]
  ,entry  "handwriting" [hand, write <> g']
  ,entryS "have"        [h <> a <> fvs']
                        [ing]
  ,entry  "he"          [h <> ee]
  ,entry  "how"         [h <> ow]] <>

  -- i section
  let inter = stks [n, t', r']
  in entries
  [entryS "I"         [ii]
                      [contractM, contractD]
  ,entry  "if"        [i <> fvs']
  ,entry  "in"        [n']
  ,entry  "input"     [n, p <> oo <> t']
  ,entry  "instant"   [n, s <> t <> a <> n' <> t']
  ,entryS "interview" [inter, v <> ew]
                      [plural]
  ,entry  "into"      [n <> t']
  ,entry  "is"        [i <> z']
  ,entryS "it"        [i <> t']
                      [plural, contractS]] <>

  -- j section
  entries
  [entry "job" [j <> o <> b']] <>

  -- k section
  entries
  [entryS "kid"   [k <> i <> d']  [ing]
  ,entry  "kitty" [k <> i <> ty']
  ,entry  "know"  [k <> n <> oe]] <>

  -- l section
  entries
  [entryS "lease"   [l <> ee <> s']
                    [ed]
  ,entry  "least"   [l <> ee <> fvs' <> t']
  ,entry  "less"    [l <> e <> s']
  ,entry  "let"     [l <> e <> t']
  ,entryS "look"    [l <> oo <> k']
                    [ing]
  ,entry  "like"    [l <> ii <> k']
  ,entry  "link"    [l <> i <> n' <> k']
  ,entryS "Lincoln" [l <> i <> n' <> k', n']
                    [contractS]
  ,entryS "log"     [l <> aw <> g']
                    [plural]
  ,entry  "ls"      [l <> s']
  ,entry  "luck"    [l <> u <> k']
  ,entry  "lucky"   [l <> u <> k' <> ee]] <>

  -- m section
  let may = m <> aa
      mod = m <> o <> d'
  in entries
  [entryS "mail"    [mail]
                    [ing]
  ,entry  "made"    [m <> aa <> d']
  ,entry  "main"    [may <> n']
  ,entry  "Maine"   [may, n']
  ,entry  "maintain" [may <> n', t <> aa <> n']
  ,entry  "male"    [rep [A, E] mail]
  ,entry  "may"     [may]
  ,entry  "me"      [m <> ee]
  ,entryS "meet"    [m <> ee <> t'] [ing]
  ,entry  "mic"     [m <> ii, k']
  ,entry  "Mike"    [m <> ii <> k']
  ,entry  "mod"      [m <> o <> d']
  ,entry  "modify"   [m <> o <> d', f <> ii]
  ,entry  "modifier" [m <> o <> d', f <> ii <> r']
  ,entry  "mom"     [m <> o <> m']
  ,entry  "more"    [m <> oe <> r']
  ,entryS "morning" [m <> oe <> r' <> ng']
                    [plural, contractS]
  ,entryS "mount"   [m <> ow <> n' <> t']
                    [plural]
  ,entry  "Microsoft" [m <> s']
  ,entry  "MS"      [m, s]
  ,entry  "much"    [m <> u <> ch']
  ,entry  "my"      [m <> ii]] <>

  -- n section
  entries
  [entry  "no"     [n <> oe]
  ,entry  "noon"   [noon]
  ,entry  "not"    [n <> o <> t']
  ,entry  "now"    [n <> ow]
  ,entryS "number" [n <> u <> m', b <> r']
                   [plural, ed, ing]] <>

  -- o section
  entries
  [entry  "of"       [u <> fvs']
  ,entry  "occur"    [k <> er]
  ,entryS "oil"      [oi <> l']
                     [ing, ed]
  ,entry  "on"       [o <> n']
  ,entry  "only"     [oe <> n' <> ly']
  ,entry  "or"       [o <> r']
  ,entryS "ordinary" [o <> r' <> d', n <> e <> ry']
                     [ly]
  ,entry  "our"      [ow <> r']
  ,entry  "out"      [ow <> t']
  ,entry  "own"      [oe <> n']] <>

  -- p section
  let per = stks [p, r']
  in entries
  [entry  "parent"  [p <> e <> r', n' <> t']
  ,entry  "per"     [per]
  ,entry  "perfect" [per, f <> e <> k' <> t']
  ,entry  "plural"  [p <> l <> oo <> r' <> l']
  ,entry  "pool"    [p <> ew <> l']
  ,entryS "present" [p <> r <> e <> z' <> n' <> t']
                    [ing, ed, ly]
  ,entry  "pretty"  [p <> r <> e <> ty']
  ,entry  "pull"    [p <> oo <> l']
  ,entry  "push"    [p <> oo <> sh']] <>

  -- q section
  entries
  [] <>

  -- r section
  entries
  [entryS "reason"  [r <> ee <> fvs' <> n']
                    [plural]
  ,entry  "real"    [r <> ee <> l']
  ,entry  "renew"   [r, n <> ew]
  ,entry  "renewal" [r, n <> ew <> l']
  ,entry  "require" [r <> ee, k <> w <> ii <> r']
  ,entry  "right"   [r <> ii <> t']
  ,entry  "rog"     [r <> o <> g']
  ,entry  "run"     [r <> u <> n']] <>

  -- s section
  let stew = stks [s, t, ew]
      sure = stks [s, oo, r']
  in entries
  [entry  "says"       [s <> e <> z']
  ,entry  "see"        [see]
  ,entry  "sin"        [s <> i <> n']
  ,entry  "sign"       [s <> ii <> n']
  ,entry  "so"         [s <> oe]
  ,entry  "some"       [s <> u <> m']
  ,entryS "sound"      [s <> ow <> nd']
                       [plural]
  ,entry  "stack"      [s <> t <> a <> k']
  ,entry  "status"     [s <> t <> a <> t' <> s']
  ,entry  "statistics" [s <> t <> a <> t' <> z']
  ,entry  "stew"       [stew]
  ,entry  "steward"    [stew <> d']
  ,entryS "string"     [s <> t <> r <> g']
                       [plural]
  ,entry  "strong"     [s <> t <> r <> aw <> ng']
  ,entry  "stronger"   [s <> t <> r <> aw <> ng', r']
  ,entry  "Stuart"     [stew <> r <> t']
  ,entry  "Stewart"    [stew, r <> t']
  ,entryS "student"    [stew, dnt]
                       [plural, contractS, pluralPosessive]
  ,entry  "success"    [s <> k', s <> e <> s']
  ,entryS "sure"       [sure]
                       [ly]
  ,entryS "surround"   [s <> r <> ow <> n' <> d'] [plural]
  ,entryS "switch"     [s <> w <> i <> ch']
                       [plural]
  ,entry  "system"     [s <> i <> s', t <> m']] <>

  -- t section
  let thank = stks [th, a, ng', k']
      tray  = stks [t, r, aa]
  in entries
  [entry  "tab"   [t <> a <> b']
  ,entry  "test"  [t <> e <> fvs' <> t']
  ,entryS "thank" [thank]
                  [plural, ing, ed]
  ,entry  "that"  [dh <> a <> t']
  ,entry  "the"   [dh']
  ,entryS "their" [dh <> e <> r']
                  [plural]
  ,entryS "there" [dh <> r']
                  [contractS]
  ,entry  "they"  [dh <> aa]
  ,entry  "they're" [dh <> aa <> r']
  ,entry  "thing" [th <> i <> g']
  ,entry  "think" [th <> i <> n' <> k']
  ,entry  "this"  [dh <> i <> s']
  ,entry  "till"  [till]
  ,entryS "time"  [t <> ii <> m']
                  [plural]
  ,entry  "to"    [t <> ew]
  ,entry  "trace" [tray <> s']
  ,entry  "Tracy" [tray, see]
  ,entry  "tray"  [tray]
  ,entry  "true"  [t <> r <> ew]] <>

  -- u section
  let up = u <> p'
  in entries
  [entry  "under"    [u <> n', d <> r']
  ,entry  "until"    [n, till]
  ,entry  "up"       [up]
  ,entry  "upcoming" [up, k <> o <> m' <> g']
  ,entry  "use"      [y <> ew <> s']] <>

  -- v section
  let val = v <> a <> l'
      ver = stks [v <> er]
  in entries
  [entry  "val"      [val]
  ,entry  "value"    [val, you]
  ,entry  "version"  [ver <> shn']
  ,entry  "virgin"   [ver, j <> n']
  ,entry  "Virginia" [v <> r', j <> n', y]] <>

  -- w section
  let well  = stks [w, e, l']
  in entries
  [entry  "want"    [w <> aw <> n' <> t']
  ,entry  "was"     [w <> u <> z']
  ,entry  "we"      [w <> ee]
  ,entry  "we've"   [w <> ee <> fvs']
  ,entry  "well"    [well]
  ,entryS "welcome" [well, come]
                    [ing]
  ,entry  "were"    [w <> er]
  ,entry  "what"    [wh <> u <> t']
  ,entry  "which"   [wh <> i <> ch']
  ,entry  "while"   [wh <> ii <> l']
  ,entry  "who"     [h <> ew]
  ,entry  "will"    [w <> i <> l']
  ,entry  "with"    [w <> i <> dh']
  ,entryS "wonder"  [w <> u <> n', d <> r']
                    [plural, ed, ing]
  ,entry  "would"   [w <> o <> d']
  ,entry  "world"   [w <> er <> l' <> d']
  ,entry  "worth"   [w <> er <> th']
  ,entryS "write"   [write]
                    [ing]
  ,entry  "written" [w <> r <> i <> t' <> n']
  ,entry  "wrote"   [r <> oe <> t']] <>

  -- x section
  entries
  [] <>

  -- y section
  entries
  [entry  "you"  [you]
  ,entry  "your" [y <> o <> r']] <>

  -- z section
  entries
  [entryS "zap" [z <> a <> p']
                [ed]]
