{-# LANGUAGE RankNTypes #-}
module Primary where

import qualified Prelude as P
import Prelude ((<>), ($))

import Dictionary
import Sounds
import Stroke
import Suffixes
import Keys
import qualified Keys.Left as L
import qualified Keys.Right as R


-- Super non-standard override, but
-- makes the entries *much* easier to read.
(.) = (<>)
infixr 9 .

sstk w =
  let selection = P.map (\(Entry _ s) -> s) $ P.filter (\(Entry n _) -> n P.== w) primaryDictionary
      len = P.length selection
  in if len P.== 1
       then P.head selection
       else P.error $ P.show len <> " entries found for \"" <> w <> "\""

(++) w1 w2 = sstk w1 <> sstk w2

(+) = go P.. sstk
  where go  [] s = [s]
        go [a] s = [a . s]
        go  ls s = P.init ls <> [P.last ls . s]
infixr 5 +

(+:) w1 s = sstk w1 <> [s]
infixr 5 +:

(^) = (P.. sstk) P.. go
  where go s  [] = [s]
        go s [a] = [s . a]
        go s  ls = s . P.head ls : P.init ls
infixl 5 :

(^:) s w1 = s : sstk w1
infixl 5 ^:

primaryDictionary =
  let -- Suffixes
      dnt  = d.n'.t'
      bl'  = b'.l'
      ly'  = l'.ee
      nd'  = n'.d'
      ry'  = r'.ee
      st'  = fvs'.t'
      ty'  = t'.ee
  in entries
  [entry "{^}{#Return}{^}{MODE:RESET}" [r.r'.b']
  ,entry "{^}\\n{^}{-|}" [r.r']
  ,entry "{^}\\n\\n{^}{-|}" [w.r.r'.b']] <>

  -- a section
  let after = stks [a, fvs', t', r']
      agree = g.r.ee
      any   = n.ee
  in entries
  [entry  "a"         [aa]
  ,entryS "Aaron"     [e.r'.n']
                      [contractS]
  ,entry  "able"      [aa.b'.l']
  ,entry  "about"     [b.ow.t']
  ,entry  "ace"       [aa.s']
  ,entry  "actually"  [a.k', ch.ew.ly']
  ,entry  "add"       [a.d']
  ,entry  "after"     [after]
  ,entryS "afternoon" (after ^: "noon")
          [plural]
  ,entry  "against"   [g.e.n'.s'.t']
  ,entry  "agree"     [agree]
  ,entry  "agreement" [agree, m.n'.t']
  ,entry  "all"       [aw.l']
  ,entry  "allowance" [l.ow.n'.s']
  ,entry  "almost"    [aw.l', m.oe.st']
  ,entry  "among"     [m.u.ng']
  ,entry  "amount"    [m, ow.n'.t'] -- m',?
  ,entry  "an"        [a.n']
  ,entry  "and"       [nd']
  ,entry  "anger"     [g.r.r']
  ,entry  "announce"  [n, ow.n'.s'] -- n'?
  ,entry  "another"   [n.u.dh'.r']
  ,entry  "any"       [any]
  ,entry  "anybody"   ("any" ++ "body")
  ,entry  "anything"  ("any" ++ "thing")
  ,entry  "anyhow"    ("any" ++ "how")
  ,entry  "anyone"    ("any" ++ "won")
  ,entry  "anywhere"  ("any" ++ "where")
  ,entryS "apparent"  [p.e.r'.n'.t']
          [ly]
  ,entry  "ape"       [aa.p']
  ,entry  "April"     [aa, p.r'.l']
  ,entry  "are"       [a.r']
  ,entry  "arm"       [a.r'.m']
  ,entry  "as"        [a.z']
  ,entry  "ash"       [a.sh']
  ,entry  "ask"       [a.fvs'.k']
  ,entry  "at"        [a.t']
  ,entry  "available" [v.aa.l', b'.l']
--  ,entry  "available" [v.aa.b'.l'] -- Potential brief
  ,entryS "average"   [a.fvs'.r'.g']
          [plural]
  ,entry  "awful"     [aw.fvs'.l']
  ,entry  "Azure"     [a.z', r']] <>

  -- b section
  let br = b.r
  in entries
  [entry  "balm"      [b.aw.m']
  ,entry  "balmy"     ("balm" + ee)
  ,entry  "be"        [b.ee]
  ,entry  "became"    (b ^: "came")
  ,entry  "because"   (b ^: "cause")
  ,entryS "become"    (b ^: "come")
          [plural]
  ,entry  "been"      [b.i.n']
  ,entry  "before"    [b, f.oe.r']
  ,entryS "begin"     [b, g.i.n']
          [ing]
  ,entry  "beige"     [b.aa.zh']
  ,entry  "being"     [b.ee.ng']
  ,entry  "belong"    [b, l.o.ng']
  ,entry  "bento"     [b.i.n', t.oe]
  ,entry  "best"      [b.e.st']
  ,entry  "bimodal"   [b.ii, m.oe, d.l']
  ,entryS "bin"       [b.i, n']
          [ed]
  ,entry  "birth"     [b.er.th']
  ,entry  "bitch"     [b.i.ch']
  ,entry  "blame"     [b.l.aa.m']
  ,entry  "bleach"    [b.l.ee.ch']
  ,entry  "blite"     [b.l.ii, t']
  ,entry  "blithe"    [b.l.ii.dh']
  ,entry  "blush"     [b.l.u.sh']
  ,entry  "board"     [b.o.r', d']
  ,entry  "body"      [b.o.d', ee]
  ,entry  "bomb"      [b.aw.m', b']
  ,entry  "bored"     [b.o.r'.d']
  ,entry  "botch"     [b.o.ch']
  ,entry  "bottom"    [b.o.t'.m']
  ,entry  "bread"     [b.r.e.d']
  ,entryS "break"     [br.aa.k']
          [ing]
  ,entry  "breathe"   [br.ee.th']
  ,entry  "breath"    [br.e.th']
  ,entry  "bring"     [br.g']
  ,entry  "broadcast" [br.o.d', k.a.st']
  ,entry  "bruise"    [br.ew.s']
  ,entry  "brush"     [br.u.sh']
  ,entry  "brutal"    [br.ew.t'.l']
  ,entryS "budget"    [b.u.j'.t']
          [plural]
  ,entryS "build"     [b.i.l'.d']
          [ing]
  ,entry  "bush"      [b.oo.sh]
  ,entry  "but"       [b.u.t']
  ,entry  "butter"    [b.u.t', r']
  ,entry  "buyer"     [b.ii.r']
  ,entry  "by"        [b.ii]] <>

  -- c section
  let car   = stks [k, aw, r']
      cert  = s.er.t'
      come  = stks [k, o, m']
      con   = k.o.n'
      cr    = k.r
  in entries
  [entry  "cab"       [k.a.b']
  ,entry  "cad"       [k.a.d']
  ,entry  "call"      [k.aw.l']
  ,entryS "calm"      [k.aw, m']
          [ed]
  ,entry  "came"      [k.aa.m']
  ,entry  "can"       [k.a.n']
  ,entry  "car"       [car]
  ,entry  "carriage"  [k.a.r'.j']
  ,entry  "Carly"     [car, ly']
  ,entry  "castle"    [k.a.fvs'.l']
  ,entry  "cat"       [k.a.t']
  ,entry  "catch"     [k.a.ch']
  ,entry  "caught"    [k.aw.t']
  ,entry  "cause"     [k.aw.z']
  ,entry  "cd"        [k.r.d'] -- as in, fingerspelling 'c'
  ,entry  "cease"     [s.ee.s']
  ,entry  "cement"    [s.m.e.n'.t']
  ,entry  "cert"      [cert]
  ,entry  "certification" [cert, f.k', aa.sh'.n']
  ,entry  "chaff"     [ch.a.fvs']
  ,entry  "chance"    [ch.a.n'.s']
  ,entry  "change"    [ch.aa.n'.g']
  ,entry  "channel"   [ch.a.n'.l']
  ,entry  "chant"     [ch.a.n'.t']
  ,entry  "charm"     [ch.a.r'.m']
  ,entry  "chart"     [ch.a.r'.t']
  ,entry  "chasm"     [k.a.fvs'.m']
  ,entry  "chat"      [ch.a.t']
  ,entry  "check"     [ch.e.k']
  ,entry  "chess"     [ch.e.s']
  ,entry  "chest"     [ch.e.st']
  ,entry  "chin"      [ch.i.n']
  ,entry  "chive"     [ch.ii.fvs']
  ,entry  "choose"    [ch.oo.s']
  ,entry  "chop"      [ch.o.p']
  ,entry  "chore"     [ch.oe.r']
  ,entry  "chrome"    [k.r.oe.m']
  ,entry  "chuck"     [ch.u.k']
  ,entry  "churn"     [ch.er.n']
  ,entry  "coat"      [k.oe.t']
  ,entry  "cod"       [k.o.d']
  ,entry  "code"      [k.oe.d']
  ,entry  "cog"       [k.o.g']
  ,entry  "coil"      [k.oi.l']
  ,entryS "come"      [come]
          [ing]
  ,entry  "commend"   [k.m.e.n'.d']
  ,entry  "commit"    [k.m.i.t']
  ,entry  "con"       [con]
  ,entryS "conflict"  [con, f.l.i.k'.t']
          [ed, ing]
  ,entryS "continue"  [k.n', t.i.n', y] [ed]
  ,entry  "contract"  [con, t.r.a.k'.t']
  ,entry  "cock"      [k.aw.k']
  ,entry  "coke"      [k.oe.k']
  ,entry  "cop"       [k.o.p']
  ,entry  "cope"      [k.oe.p']
  ,entryS "copy"      [k.aw.p'.ee]
          [plural]
  ,entry  "core"    [k.oe.r']
  ,entry  "correct" [k.r', e.k'.t']
  ,entryS "corrupt" [k.r, u.p'.t']
          [ed]
  ,entry  "cot"     [c.o.t']
  ,entry  "could"   [k.oo.d']
  ,entry  "course"  [k.o.r'.s']
  ,entry  "cover"   [k.o.fvs'.r']
  ,entry  "crap"    [cr.a.p']
  ,entry  "crash"   [cr.a.sh']
  ,entry  "crawl"   [cr.aw.l']
  ,entry  "cred"    [cr.e.d']
  ,entryS "credit"  [cr.e.d', t']
          [ed]
  ,entry  "crew"    [cr.ew]
  ,entry  "cringe"  [cr.i.n, j']
  ,entryS "cross"   [cr.aw.s']
          [ed]
  ,entryS "crow"    [cr.oe]
          [ed, ing]
  ,entry  "crude"   [cr.ew.d']
  ,entry  "cruel"   [cr.ew.l']
  ,entry  "cruise"  [cr.ew.z']
  ,entryS "crush"   [cr.u.sh']
          [ed, ing]
  ,entry  "crutch"  [cr.u.ch']
  ,entry  "crystal" [k.r.i.fvs'.t'.l']
  ,entry  "cub"     [k.u.b']
  ,entry  "cube"    [k, y.ew.b']
  ,entry  "cuff"    [k.u.fvs']
  ,entry  "cull"    [k.u.l']
  ,entry  "cunt"    [k.u.n'.t']
  ,entry  "cup"     [k.u.p']
  ,entry  "cure"    [k, y.oo.r']
  ,entry  "curse"   [k.er.s']
  ,entryS "cuss"    [k.u.s']
          [ed, ing]
  ,entry  "cut"     [k.u.t']
  ,entry  "cute"    [k, y.ew.t']] <>

  -- d section
  let day = d.aa
      drain = d.r.aa.n
  in entries
  [entry  "dabble" [d.a.b'.l']
  ,entry  "dance" [d.a.n'.s']
  ,entry  "dash" [d.a.sh']
  ,entryS "date"   [day.t']
          [plural]
  ,entryS "day"    [day]
          [plural]
  ,entry  "deaf"  [d.e.fvs']
  ,entry  "death" [d.e.th']
  ,entry  "debt" [d.e.t']
  ,entry  "deference" [d.e.fvs'.r'.n'.s']
  ,entry  "defer" [d.f.er]
  ,entry  "deferral" [d.f.er.l]
  ,entry  "delish" [d.l.i.sh']
  ,entry  "delicious" [d.l.i.sh'.s']
  ,entry  "demand" [d.m.a.nd']
  ,entry  "den"    [d.e.n']
  ,entryS "dent"   [d.e.n'.t']
          [plural]
  ,entry  "depress" [d.p.r.e.s']
  ,entry  "derange" [drain, j']
  ,entryS "describe" (d ^: "scribe")
          [ed]
  ,entry  "design" [d, z.ii.n']
  ,entry  "desire" [d, z.ii.r']
  ,entry  "dessert" [d, z.er.t']
  ,entryS "destroy" [d, s.t.r.oi]
          [ed, ing, plural]
  ,entry  "devil" [d.e.fvs'.l']
  ,entry  "dick"  [d.i.k']
  ,entry  "diff"   [d.i.fvs']
  ,entry  "different" [d.i.fvs'.r'.n'.t']

  ,entry  "digit" [d.i.j'.t']
  ,entry  "din"   [d.i.n']
  ,entry  "dine"  [d.ii.n']
  ,entry  "diner" [d.ii.n', r']
  ,entry  "dinner" [d.i.n', r']
  ,entry  "ditch" [d.i.ch']
  ,entry  "do"     [d.ew]
  ,entry  "don't"  [d.oe.n'.t']
  ,entry  "does"   [d.u.z']
  ,entry  "doesn't" [d.u.z', n'.t']
  ,entry  "done"   [d.u.n']
  ,entry  "donor" [d.oe.n', r']
  ,entryS "double" [d.u.b'.l']
          [ing, ed, plural]
  ,entryS "drag"  [d.r.a.g']
          [plural, ed, ing]
  ,entry  "drain"  [drain]
  ,entry  "dream" [d.r.ee.m']
  ,entry  "drug" [d.r.u.g']
  ,entry  "duck" [d.u.k']
  ,entry  "due" [d, ew]
  ,entry  "duffer" [d.u.fvs'.r']
  ,entry  "dung" [d.u.ng']] <>

  -- e section
  entries
  [entry  "ear"    [ee.r']
  ,entry  "early"  [er.l']
  ,entry  "earn"   [er.n']
  ,entry  "earns"  [er.n'.z']
  ,entry  "earth"  [er.th']
  ,entry  "ease"   [ee.z']
  ,entry  "easy"   [ee.z', ee]
  ,entryS "eat"    [ee.t']
          [ing]
  ,entryS "echo"   [e.k']
          [plural]
  ,entryS "edge"   [e.j']
          [plural
          ,ed
          ,ing]
  ,entryS "edit"   [e.d',t']
          [plural
          ,ed
          ,ing]
  ,entryS "eel"    [ee.l']
          [plural]
  ,entryS "effort" [f.r'.t']
          [plural]
  ,entryS "Elliot"    [e.l'.t']
          [contractS]
  ,entry  "eke" [ee.k']
  ,entry  "elf" [l.fvs']
  ,entry  "else"      [e.l'.s']
  ,entry  "elves" [l.fvs'.z']
  ,entryS "email"     (ee ^: "mail")
          [ing]
  ,entry  "emacs"     [ee, m.a.x']
  ,entry  "emergency" [m.er.g', n.s'.ee]
  ,entry  "Emily"     [e.m, ly']
  ,entryS "enact" [n.a.k'.t'] [plural]
  ,entryS "entry"     [n, t.r.ee] [plural]
  ,entry  "etch" [e.ch']
  ,entry  "even" [ee.fvs'.n']
  ,entry  "ever" [e.fvs'.r']
  ,entry  "every" [e.fvs'.r', ee]
  ,entry  "evil" [ee.fvs'.l']
  ,entry  "exam"    [x.a.m']
  ,entry  "example" [x.a.m', p'.l']
  ,entry  "except"    [x.e.p'.t']] <>

  -- f section
  let fee = f.ee
  in entries
  [entry  "fable"   [f.aa.bl']
  ,entryS "faculty" [f.a.k'.l'.ty']
                    [contractS]
  ,entry  "false"   [f.aw.l'.s']
  ,entry  "fang" [f.aa.ng']
  ,entry  "fee"  [fee]
  ,entry  "feeble" [fee.bl']
  ,entry  "feral" [f.e.r'.l']
  ,entry  "fetch" [f.e.ch']
  ,entry  "fidget" [f.i.j'.t']
  ,entry  "fierce" [f.ee.r'.s']
  ,entry  "fifth" [f.i.fvs'.th']
  ,entry  "find"    [f.ii.nd']
  ,entry  "finger" [f.g', r']
  ,entry  "fire"    [f.ii.r']
  ,entry  "fish" [f.i.sh']
  ,entry  "flash" [f.l.a.sh']
  ,entry  "flaunt" [f.l.aw.n'.t']
  ,entry  "flesh" [f.l.e.sh']
  ,entry  "float"   [f.l.oe.t']
  ,entry  "flung" [f.l.u.ng']
  ,entry  "flush" [f.l.u.sh']
  ,entry  "for"     [f.o.r']
  ,entry  "ford" [f.o.r'.d']
  ,entry  "forge" [f.o.r'.j']
  ,entryS "form" [f.o.r'.m'] [plural]
  ,entry  "free"    [f.r.ee]
  ,entry  "fresh" [f.r.e.sh']
  ,entry  "fries" [f.r.ii.z']
  ,entry  "frigid" [f.r.i.j'.d']
  ,entry  "fringe" [f.r.i.n, j']
  ,entry  "from"    [f.r.o.m']
  ,entry  "frost"   [f.r.o.st']
  ,entry  "fuck"    [f.u.k']] <>

  -- g section
  entries
  [entry  "gadget"    [g.a.j'.t']
  ,entry  "gang"      [g.a.ng']
  ,entry  "garage"    [g.r.aw.zh']
  ,entry  "gash"      [g.a.sh']
  ,entryS "germ"      [j.e.r'.m'] [plural]
  ,entryS "gerund"    [j.e.r', n'.d'] [plural]
  ,entry  "get"       [g.e.t']
  ,entry  "gift"      [g.i.fvs'.t']
  ,entry  "git"       [g.i.t']
  ,entry  "give"      [g.i.fvs']
  ,entry  "given"     [g.i.fvs'.n']
  ,entry  "glad"      [g.l.a.d']
  ,entry  "glare"     [g.l.aa.r']
  ,entryS "glean"     [g.l.ee.n'.s'] [plural]
  ,entryS "glide"     [g.l.ii.d'] [plural]
  ,entry  "glitch"    [g.l.i.ch']
  ,entry  "gnome"     [n.oe.m']
  ,entry  "goblet"    [g.o.b'.l'.t']
  ,entry  "good"      [g.oo.d']
  ,entry  "gosh"      [g.o.sh']
  ,entry  "gram"      [g.r.a.m']
  ,entry  "grammar"   [g.r.a.m', r']
  ,entry  "grant"     [g.r.a.n'.t']
  ,entry  "gravel"    [g.r.a.fvs'.l']
  ,entry  "great"     [g.r.aa.t']
  ,entry  "group"     [g.r.ew.p']
  ,entry  "growth"    [g.r.oe.th']
  ,entry  "grudge"    [g.r.u.j']
  ,entry  "grunge"    [g.r.u.n', j']
  ,entry  "grunt"     [g.r.u.n'.t']
  ,entryS "guarantee" [g.e.r'.n'.ty']
                      [ed, plural]
  ,entry  "guest"     [g.e.st']
  ,entry  "guide"     [g.ii.d']
  ,entry  "gym"       [j.i.m']] <>

  -- h section
  let hand = stks [h.a.n'.d']
  in entries
  [entry  "had"         [h.a.d']
  ,entry  "hag" [h.a.g']
  ,entry  "half" [h.a, fvs']
  ,entry  "hall" [h.aw.l']
  ,entry  "halt" [h.aw.l'.t']
  ,entry  "hand"        [hand]
  ,entry  "handwriting" (hand ^: "write{^ing}")
  ,entry  "harm" [h.aw.r'.m']
  ,entry  "has"         [h.a.z']
  ,entry  "hash" [h.a.sh']
  ,entry  "hat" [h.a.t']
  ,entry  "hatch" [h.a.ch']
  ,entry  "hath" [h.a.th']
  ,entryS "have"        [h.a.fvs']
                        [ing]
  ,entry  "he"          [h.ee]
  ,entry  "head"        [h.e.d']
  ,entry  "heal"        [h.ee.l']
  ,entry  "health"      [h.e.l'.th']
  ,entry  "heap"        [h.ee.p']
  ,entry  "hear"        [h.ee.r']
  ,entry  "heat"        [h.ee.t']
  ,entry  "heel"        [h.ee'.l']
  ,entry  "height"      [h.ii.t']
  ,entry  "hell"        [h.e.l']
  ,entry  "her"         [h.er]
  ,entry  "here"        [h.ee'.r']
  ,entry  "hermit"      [h.er.m'.t']
  ,entry  "hid"         [h.i.d']
  ,entry  "hinge"       [h.i.n, j']
  ,entry  "hit"         [h.i.t']
  ,entry  "hoary"       ("whore" + ry')
  ,entry  "hoarfrost"   ("whore" ++ "frost")
  ,entry  "hog"         [h.o.g']
  ,entry  "hood"        [h.oo.d']
  ,entry  "hoe"         [h.oe]
  ,entry  "hop"         [h.o.p']
  ,entry  "hope"        [h.oe.p']
  ,entry  "horrible"    ("whore" + bl')
  ,entry  "horror"      ("whore" +: r')
  ,entry  "host" [h.oe.st']
  ,entry  "hot" [h.o.t']
  ,entry  "house" [h.ow.s']
  ,entry  "hover" [h.o.fvs'.r']
  ,entry  "how"         [h.ow]
  ,entry  "howl"        ("how" + l')
  ,entry  "hub" [h.u.b']
  ,entry  "huff" [h.u.fvs']
  ,entry  "hug" [h.u.g']
  ,entry  "hull" [h.u.l']
  ,entry  "hunt" [h.u.n'.t']
  ,entry  "hush" [h.u.sh']
  ,entry  "hut" [h.u.t']
  ,entry  "hutch" [h.u.ch']] <>

  -- i section
  let inter = stks [n, t', r']
  in entries
  [entryS "I"         [ii]
                      [contractM, contractD]
  ,entry  "ice"       [ii.s']
  ,entryS "idea"      [d.ee]
                      [plural]
  ,entry  "if"        [i.fvs']
  ,entry  "in"        [n']
  ,entryS "include"   [n, k.l.ew.d'] [ed, ing, plural]
  ,entry  "inhibit"   [n, h.i.b'.t']
  ,entry  "input"     [n, p.oo.t']
  ,entry  "instant"   [n, s.t.a.n'.t']
  ,entry  "interest"  [n, t.r.e.st']
  ,entryS "interview" [inter, v.ew]
                      [plural]
  ,entry  "into"      [n.t']
  ,entry  "ire"       [ii.r']
  ,entry  "Irish"     [ii, r.sh']
  ,entry  "is"        [i.z']
  ,entryS "it"        [i.t']
                      [plural, contractS]] <>

  -- j section
  entries
  [entry  "jagged"  [j.a.g'.d']
  ,entry  "jam"     [j.a.m']
  ,entry  "jazz"    [j.a.z']
  ,entry  "jest"    [j.e.st']
  ,entry  "jets"    [j.e.t'.s']
  ,entry  "job"     [j.o.b']
  ,entry  "join"    [j.oi.n']
  ,entry  "jostle"  [j.o.fvs'.l']
  ,entry  "journal" [j.er.n'.l']
  ,entry  "joy"     [j.oi]
  ,entry  "joyful"  [j.oi.fvs'.l']
  ,entry  "judge"   [j.u.j']
  ,entry  "jug"     [j.u.g']
  ,entry  "just"    [j.u.st']
  ,entry  "jut"     [j.u.t']] <>

  -- k section
  entries
  [entry  "keg"   [k.e.g']
  ,entryS "kid"   [k.i.d'] [ing]
  ,entry  "kite"  [k.ii.t']
  ,entry  "knack" [n.a.k']
  ,entry  "knock" [k.n.aw.k']
  ,entry  "kitty" [k.i.ty']
  ,entry  "knew"   [k.n.ew]
  ,entry  "knight" [k.n.ii.t']
  ,entry  "know"  [k.n.oe]
  ,entry  "known" [n.oe.n']
  ,entryS "knurle" [n.er.l']
                   [ed, ing]] <>

  -- l section
  let lee = l.ee
  in entries
  [entry  "lace"     [l.aa.s']
  ,entry  "lash"     [l.a.sh']
  ,entry  "latch"    [l.a.ch']
  ,entryS "lead"     [lee.d']
          [plural, ing]
  ,entryS "lead"     [l.e.d']
          [ed]
  ,entryS "leader"   [lee.d', r']
          [plural]
  ,entryS "learn"    [l.er.n']
          [ed]
  ,entryS "lease"    [lee.s']
          [ed]
  ,entry  "least"    [lee.st']
  ,entry  "lee"      [lee]
  ,entry  "leech"    [lee.ch']
  ,entry  "lend"     [l.e.nd']
  ,entry  "less"     [l.e.s']
  ,entry  "let"      [l.e.t']
  ,entry  "letter"   [l.e.t', r']
  ,entry  "leverage" [l.e.fvs', r'.j']
  ,entryS "look"     [l.oo.k']
          [ing]
  ,entry  "lounge"   [l.ow.n', j']
  ,entryS "lover"    [l.u.fvs'.r']
          [plural]
  ,entry  "like"     [l.ii.k']
  ,entry  "link"     [l.i.n'.k']
  ,entryS "Lincoln"  ("link" +: n')
          [contractS]
  ,entryS "log"      [l.aw.g']
          [plural]
  ,entry  "loin"     [l.oi.n']
  ,entry  "ls"       [l.s']
  ,entry  "luck"     [l.u.k']
  ,entry  "lucky"    ("luck" + ee)
  ,entry  "lung"     [l.u.ng']
  ,entry  "lust"     [l.u.st']] <>

  -- m section
  let ma  = m.a 
      mee = m.ee
      may = m.aa
      mod = m.o.d'
  in entries
  [entry  "maggot"    [ma.g'.t']
  ,entry  "maim"      [may.m']
  ,entryS "mail"      [may.l']
          [ing]
  ,entry  "made"      [may.d']
  ,entry  "main"      [may.n']
  ,entry  "Maine"     [may, n']
  ,entry  "maintain"  [may.n', t.aa.n']
  ,entry  "male"      [may, l']
  ,entry  "mallet"    [ma.l'.t']
  ,entry  "malt"      [m.aw.l'.t']
  ,entry  "many"      [m.e, n'.ee]
  ,entry  "mash"      [ma.sh']
  ,entry  "mat"       [ma.t']
  ,entry  "match"     [ma.ch']
  ,entry  "math"      [ma.th']
  ,entry  "matter"    [ma.t', r']
  ,entry  "may"       [may]
  ,entry  "me"        [mee]
  ,entry  "meal"      [mee.l']
  ,entryS "meet"      [mee.t'] [ing]
  ,entry  "merge"     [m.er.j']
  ,entry  "mesh"      [m.e.sh']
  ,entry  "meth"      [m.e.th']
  ,entry  "method"    [m.e.th'.d']
  ,entry  "mic"       [m.ii, k']
  ,entry  "Microsoft" [m.s']
  ,entry  "Mike"      [m.ii.k']
  ,entry  "mod"       [m.o.d']
  ,entry  "modify"    ("mod" +: f.ii)
  ,entry  "modifier"  ("mod" +: f.ii.r')
  ,entry  "mom"       [m.o.m']
  ,entry  "mope"      [m.oe.p']
  ,entry  "morals"    ("more" + l'.s')
  ,entry  "more"      [m.o.r']
  ,entryS "morning"   ("more" + ng')
          [plural, contractS]
  ,entry  "mound"     [m.ow.n'.d']
  ,entryS "mount"     [m.ow.n'.t']
          [plural]
  ,entry  "move"      [m.ew.fvs']
  ,entry  "MS"        [m, s]
  ,entry  "much"      [m.u.ch']
  ,entry  "muffle"    [m.u.fvs'.l']
  ,entry  "must"      [m.u.st']
  ,entry  "my"        [m.ii]] <>

  -- n section
  let nest = n.e.fvs'.t'
  in entries
  [entry  "nab"    [n.a.b']
  ,entry  "nag"    [n.a.g']
  ,entryS "name"   [n.aa.m'] [ed]
  ,entry  "nap"    [n.a.p']
  ,entry  "neck"   [n.e.k']
  ,entry  "nerd"   [n.er.d']
  ,entry  "nest"   [nest]
  ,entry  "nestle" [nest, l']
  ,entry  "neural" [n.oo.r'.l']
  ,entry  "never"  [n.e.fvs'.r']
  ,entry  "new"    [n.ew]
  ,entry  "no"     [n.oe]
  ,entry  "nock"   [n.o.k']
  ,entry  "nod"    [n.o.d']
  ,entry  "noise"  [n.oi.z']
  ,entry  "noon"   [n.ew.n']
  ,entry  "not"    [n.o.t']
  ,entry  "notes"  [n.oe.t'.s']
  ,entry  "noun"   [n.ow.n']
  ,entry  "novice" [n.o.fvs'.s']
  ,entry  "now"    [n.ow]
  ,entry  "nub"    [n.u.b']
  ,entry  "nudge"  [n.u.j']
  ,entryS "number" [n.u.m', b.r']
                   [plural, ed, ing]
  ,entry  "nut"    [n.u.t']] <>

  -- o section
  entries
  [entry  "oar"       [o, r']
  ,entry  "oat"       [oe.t']
  ,entry  "oath"      [oe.th']
  ,entry  "occur"     [k.er]
  ,entry  "ode"       [oe.d']
  ,entry  "of"        [u.fvs']
  ,entryS "oil"       [oi.l']
                      [ing, ed]
  ,entry  "on"        [o.n']
  ,entry  "only"      [oe.n'.ly']
  ,entry  "opinion"   [p.i.n', y.n']
  ,entry  "or"        [o.r']
  ,entry  "orange"    [aw.r', n.j']
  ,entryS "ordinary"  [o.r'.d', n.e.ry']
                      [ly]
  ,entryS "other"     [u.dh', r] [plural]
  ,entry  "otherwise" [u.dh', r, w.ii.z']
  ,entry  "our"       [ow.r']
  ,entry  "out"       [ow.t']
  ,entry  "own"       [oe.n']
  ,entry  "over"      [oe.fvs'.r']] <>

  -- p section
  let pa  = p.a
      pay = p.aa
      pee = p.ee
      per = p.r'
      pour = p.o.r'
      pr   = p.r
  in entries
  [entry  "pace"     [pay.s']
  ,entry  "pad"      [pa.d']
  ,entry  "paid"     [pay.d']
  ,entry  "pair"     [pay.r']
  ,entry  "pal"      [pa.l']
  ,entry  "palm"     [p.aw.l', m']
  ,entry  "pan"      [pa.n']
  ,entry  "panel"    [pa.n'.l']
  ,entry  "parent"   [p.e.r', n'.t']
  ,entryS "pass"     [pa.s']
          [ed, ing]
  ,entry  "past"     [pa.st']
  ,entry  "pat"      [pa.t']
  ,entry  "patch"    [pa.ch']
  ,entry  "path"     [pa.th']
  ,entry  "pay"      [pay]
  ,entryS "pea"      [p.ee']
          [plural]
  ,entry  "peace"    [pee, s']
  ,entry  "pear"     [pay, r']
  ,entryS "pee"      [pee]
          [ed
          ,ing]
  ,entry  "peer"     [pee.r]
  ,entry  "peg"      [p.e.g']
  ,entry  "pent"     [p.e.n'.t']
  ,entry  "pep"      [p.e.p']
  ,entry  "per"      [per]
  ,entry  "percent"  [per, s.e.n'.t']
  ,entry  "perfect"  [per, f.e.k'.t']
  ,entry  "pet"      [p.e.t']
  ,entry  "phrase"   [f.r.aa.z']
  ,entry  "piece"    [pee.s']
  ,entry  "pig"      [p.i.g']
  ,entry  "ping"     [p.g']
  ,entry  "Pinyin"   [p.i.n', y.i.n']
  ,entry  "pipe"     [p.ii.p']
  ,entry  "place"    [p.l.aa.s']
  ,entry  "plumb"    [p.l.u.m']
  ,entry  "plunge"   [p.l.u.n', j']
  ,entry  "plural"   [p.l.oo.r'.l']
  ,entry  "plush"    [p.l.u.sh']
  ,entry  "pod"      [p.aw.d']
  ,entry  "Poe"      [p.oe]
  ,entry  "pole"     [p.oe, l']
  ,entry  "poll"     [p.oe.l']
  ,entry  "pong"     [p.aw.ng']
  ,entry  "pool"     [p.ew.l']
  ,entryS "poot"     [p.ew.t']
          [ed]
  ,entry  "porridge" [pour, j']
  ,entry  "port"     [pour.t']
  ,entry  "portal"   [pour.t', l']
  ,entry  "pose"     [p.oe.z']
  ,entry  "posh"     [p.o.sh']
  ,entry  "post"     [p.oe.st']
  ,entry  "pot"      [p.o.t']
  ,entry  "pour"     [pour]
  ,entry  "pout"     [p.ow.t']
  ,entry  "power"    [p.ow.r']
  ,entry  "praise"   [pr.aa.z']
  ,entryS "present"  [pr.e.z', n'.t']
          [ing, ed, ly]
  ,entryS "press"    [pr.e.s']
          [ed]
  ,entry  "pretty"   [pr.e.ty']
  ,entry  "pride"    [pr.ii.d']
  ,entry  "priest"   [pr.ee.st']
  ,entryS "prime"    [pr.ii.m']
          [ed]
  ,entry  "prize"    [pr.ii.z']
  ,entry  "probably" [pr.o.b'.l'.ee]
  ,entry  "prong"    [pr.o.ng']
  ,entry  "prose"    [pr.oe.z']
  ,entry  "prowl"    [pr.ow.l']
  ,entry  "prude"    [pr.ew.d']
  ,entry  "psalm"    [s.aw.l', m']
  ,entry  "pub"      [p.u.b']
  ,entry  "puff"     [p.u.fvs']
  ,entry  "pug"      [p.u.g']
  ,entry  "pull"     [p.oo.l']
  ,entry  "pup"      [p.u.p']
  ,entry  "pure"     [p.y.u.r']
  ,entry  "purge"    [p.er.j']
  ,entryS "purr"     [p.er]
          [ed, ing]
  ,entry  "pus"      [p.u.s']
  ,entry  "push"     [p.oo.sh']
  ,entry  "put"      [p.oo.t']] <>

  -- q section
  let qual = q.o.l'
  in entries
  [entry  "quack"    [q.a.k']
  ,entry  "quake"    [q.aa.k']
  ,entry  "qual"     [qual]
  ,entry  "quality"  [qual.ty']
  ,entryS "qualm"    [qual, m']
          [plural]
  ,entry  "queen"    [q.ee.n']
  ,entry  "quest"    [q.e.st']
  ,entry  "question" [q.e.st', n']
  ,entry  "quid"     [q.i.d']
  ,entry  "quo"      [q.oe]] <>

  -- r section
  entries
  [entry  "rabble" [r.a.b'.l']
  ,entry  "race" [r.aa.s']
  ,entry  "rag" [r.a.g']
  ,entry  "raid" [r.aa.d']
  ,entry  "rail" [r.aa.l']
  ,entry  "raise" [r.aa.z']
  ,entry  "rang" [r.aa.ng']
  ,entry  "range" [r.aa.n', j']
  ,entry  "rant" [r.a.n'.t']
  ,entry  "rap" [r.a.p']
  ,entry  "rape" [r.aa.p']
  ,entry  "rash" [r.a.sh']
  ,entry  "rat" [r.a.t']
  ,entry  "ratchet" [r.a.ch'.t']
  ,entry  "rate" [r.aa.t']
  ,entry  "raw" [r.aw]
  ,entry  "ray" [r.aa]
  ,entry  "read" [r.ee'.d']
  ,entry  "real"    [r.ee'.l']
  ,entryS "reason"  [r.ee.fvs'.n'] [plural]
  ,entry  "rebel" [r.e.b'.l']
  ,entry  "rebel" [r, b.e.l']
  ,entryS "receive" [r, s.ee.fvs'] [ed, ing, plural]
  ,entry  "red" [r.e.d']
  ,entry  "reed" [r.ee.d']
  ,entry  "reel" [r.ee.l']
  ,entry  "refer" [r, f.er]
  ,entry  "renew"   [r, n.ew]
  ,entry  "renewal" [r, n.ew.l']
  ,entry  "require" [r.ee, k.w.ii.r']
  ,entry  "retch"   [r.e.ch']
  ,entryS "rhyme"   [r.ii.m']
          [plural]
  ,entry  "rid" [r.i.d']
  ,entry  "rifle" [r.ii.fvs'.l']
  ,entry  "rift" [r.i.fvs'.t']
  ,entry  "rig" [r.i.g']
  ,entry  "right"   [r.ii.t']
  ,entry  "ripe" [r.ii.p']
  ,entry  "river" [r.ii.fvs'.r']
  ,entry  "road" [r.oe.d']
  ,entry  "roar" [r.oe.r']
  ,entryS "rob" [r.o.b'] [ed]
  ,entry  "robe" [r.oe.b']
  ,entryS "rocket" [r.o.k'.t'] [plural]
  ,entry  "rod" [r.o.d']
  ,entry  "rode" [r.oe, d']
  ,entry  "rog"     [r.o.j']
  ,entry  "roll" [r.oe.l']
  ,entry  "rope" [r.oe.p']
  ,entry  "rot" [r.o.t']
  ,entry  "rough" [r.u.fvs']
  ,entry  "rove" [r.oe.fvs']
  ,entryS "rub" [r.u.b'] [ed]
  ,entry  "rubble" [r.u.b'.l']
  ,entry  "rude" [r.ew.d']
  ,entryS "ruffle" [r.u.fvs'.l'] [ed]
  ,entry  "rug" [r.u.g']
  ,entry  "rule" [r.ew.l']
  ,entry  "rum" [r.u.m']
  ,entry  "run"     [r.u.n']
  ,entry  "rung" [r.u.ng']
  ,entry  "ruse" [r.ew.z']
  ,entry  "rush" [r.u.sh']
  ,entry  "rut" [r.u.t']] <>

  -- s section
  let see  = s.ee
      sis  = s.i.s'
      some = s.u.m'
      spun = s.p.u.n'
      stew = s.t.ew
      sure = s.oo.r'
  in entries
  [entry  "sad"      [s.a.d']
  ,entry  "sag"      [s.a.g']
  ,entry  "said"     [s.e.d']
  ,entry  "sail"     [s.aa, l']
  ,entry  "sale"     [s.aa.l']
  ,entry  "sap"      [s.a.p']
  ,entry  "sass"     [s.a.s']
  ,entry  "sat"      [s.a.t']
  ,entry  "Saul"     [s.aw.l']
  ,entry  "saw"      [s.aw]
  ,entry  "says"     [s.e.z']
  ,entry  "scaffold" [s.k.a.fvs'.l'.d']
  ,entry  "scald"    [s.aw.l'.d']
  ,entry  "scarlet"  [s.k.a.r'.l'.t']
  ,entry  "school"   [s.k.ew.l']
  ,entry  "scoff"    [s.k.aw.fvs']
  ,entry  "scour"    [s.k.ow.r']
  ,entry  "scribble" [s.k.r.i.b'.l']
  ,entry  "scribe"   [s.k.r.ii.b']
  ,entryS "scuffle"  [s.k.u.fvs'.l'] [ed]
  ,entry  "sea"      [s.ee']
  ,entry  "seat"     [s.ee.t']
  ,entry  "secrete"  [s, k.r.ee.t']
  ,entry  "see"      [see]
  ,entry  "seed"     [see.d']
  ,entry  "seethe"   [see.th']
  ,entry  "seize"    [see.z']
  ,entry  "sell"     [s.e.l']
  ,entry  "semester" [s, m.e.st', r']
  ,entry  "serum"    [see.r'.m']
  ,entry  "set"      [s.e.t']
  ,entry  "several"  [s.e.fvs'.r'.l']
  ,entry  "severance" [s.e.fvs'.r'.n'.s']
  ,entry  "sew"      [s.oe.stk Star]
  ,entry  "shall"    [sh.a.l']
  ,entry  "shark"    [sh.a.r'.k']
  ,entry  "sheesh"   [sh.ee.sh']
  ,entry  "shell"    [sh.e.l']
  ,entry  "shim"     [sh.i.m']
  ,entry  "shit"     [sh.i.t']
  ,entry  "shock"    [sh.aw.k']
  ,entry  "shore"    [sh.oe.r']
  ,entryS "shovel"   [sh.u.fvs', l']
          [plural]
  ,entry  "shrewd"   [sh.r.ew.d']
  ,entry  "shrine"   [sh.r.ii.n']
  ,entry  "shrivel"  [sh.r.i.fvs'.l']
  ,entry  "shuck"    [sh.u.k']
  ,entryS "shuffle"  [sh.u.fvs'.l'] [ed]
  ,entry  "shush"    [sh.u.sh']
  ,entry  "sign"     [s.ii.n']
  ,entry  "sin"      [s.i.n']
  ,entry  "sip"      [s.i.p']
  ,entry  "sir"      [s.i.r']
  ,entry  "sketch"   [s.k.e.ch']
  ,entry  "skew"     [s.k.ew]
  ,entry  "skirt"    [s.k.r'.t']
  ,entry  "slash"    [s.l.a.sh']
  ,entry  "slew"     [s.l.ew]
  ,entry  "sleuth"   [s.l.ew.th']
  ,entry  "slime"    [s.l.ii.m']
  ,entry  "slosh"    [s.l.o.sh']
  ,entry  "slouch"   [s.l.ow.ch']
  ,entry  "slurp"    [s.l.er.p']
  ,entry  "smooch"   [s.m.ew.ch']
  ,entry  "smudge"   [s.m.u.j']
  ,entry  "smug"     [s.m.u.g']
  ,entry  "smuggle"  [s.m.u.g', l']
  ,entry  "smush"    [s.m.oo.sh']
  ,entry  "snide"    [s.n.ii.d']
  ,entry  "so"       [s.oe]
  ,entry  "soar"     [s.oe, r']
  ,entry  "sob"      [s.aw.b']
  ,entry  "soil"     [s.oi.l']
  ,entry  "sold"     [s.oe.l'.d']
  ,entry  "some"     [some]
  ,entry  "somebody"  ("some" ++ "body")
  ,entry  "somehow"   ("some" ++ "how")
  ,entry  "someone"   ("some" ++ "won")
  ,entry  "someplace" ("some" ++ "place")
  ,entry  "something" ("some" ++ "thing")
  ,entry  "sometime"  ("some" ++ "time")
  ,entry  "sometimes" ("some" ++ "time{^s}")
  ,entry  "somewhere" ("some" ++ "where")
  ,entry  "song"      [s.o.ng']
  ,entry  "soot"      [s.oo.t']
  ,entry  "sop"       [s.o.p']
  ,entry  "sore"      [s.oe.r']
  ,entryS "sound"     [s.ow.nd']
                      [plural]
  ,entry  "soy"       [s.oi]
  ,entry  "spew"      (s.p ^: "you")
  ,entry  "sphere"    [s.f.ee.r']
  ,entry  "spill"     [s.p.i.l']
  ,entry  "spite"     [s.p.ii.t']
  ,entry  "splash"    [s.p.l.a.sh']
  ,entry  "splotch"   [s.p.l.o.ch']
  ,entry  "sponge"    [spun, j']
  ,entry  "spore"     [s.p.oe.r']
  ,entryS "spot"      [s.p.o.t']
          [ed]
  ,entry  "spouse"    [s.p.ow.s']
  ,entry  "sprawl"    [s.p.r.aw.l']
  ,entry  "spray"     [s.p.r.aa]
  ,entry  "sprout"    [s.p.r.ow.t']
  ,entry  "spun"      [spun]
  ,entry  "squash"    [s.q.a.sh']
  ,entry  "squish"    [s.q.i.sh']
  ,entry  "stack"     [s.t.a.k']
  ,entry  "stair"     [s.t.aa, r']
  ,entry  "star"      [s.t.a.r']
  ,entry  "stare"     [s.t.aa.r']
  ,entryS "start"     [s.t.a.r'.t']
          [ed]
  ,entry  "stash"     [s.t.a.sh']
  ,entry  "statistics" [s.t.a.t'.z']
  ,entry  "status"     [s.t.a.t'.s']
  ,entry  "stew"       [stew]
  ,entry  "steward"    [stew.d']
  ,entry  "Stewart"    [stew, r.t']
  ,entry  "still"      (s ^ "till")
  ,entry  "store"      [s.t.o.r']
  ,entry  "storage"    [s.t.o.r'.j']
  ,entry  "storing"    [s.t.o.r'.g']
  ,entryS "strap"      [s.t.r.a.p']
          [plural
          ,ed
          ,ing]
  ,entry  "straw"      [s.t.r.aw]
  ,entry  "stress"     [s.t.r.e.s']
  ,entry  "stretch"    [s.t.r.e.ch']
  ,entryS "string"     [s.t.r.g']
                       [plural]
  ,entry  "strong"     [s.t.r.aw.ng']
  ,entry  "stronger"   [s.t.r.aw.ng', r']
  ,entry  "Stuart"     [stew.r.t']
  ,entryS "student"    [stew, dnt]
          [plural
          ,contractS
          ,pluralPosessive]
  ,entry  "stun"       [s.t.u.n']
  ,entry  "stung"      [s.t.u.ng']
  ,entry  "success"    [s.k', s.e.s']
  ,entry  "such"       [s.u.ch']
  ,entry  "sugar"      [s.h.oo.g',r']
  ,entry  "superb"     [s.p.er.b']
  ,entry  "suppress"   [s.p.r.e.s']
  ,entryS "sure"       [sure]
          [ly]
  ,entryS "surround"   [s.r.ow.n'.d']
          [plural]
  ,entryS "switch"     [s.w.i.ch']
          [plural]
  ,entry  "syringe"    [s.r.i.n', j']
  ,entry  "syrup"      [s.er.p']
  ,entry  "system"     [sis, t.m']] <>

  -- t section
  let thank = stks [th, a, ng', k']
      tray  = stks [t, r, aa]
  in entries
  [entry  "tab"     [t.a.b']
  ,entry  "teat"    [t.ee.t]
  ,entry  "test"    [t.e.fvs'.t']
  ,entryS "thank"   [thank]
                    [plural, ing, ed]
  ,entry  "that"    [dh.a.t']
  ,entry  "the"     [dh']
  ,entryS "their"   [dh.e.r']
                    [plural]
  ,entryS "there"   [dh.r']
                    [contractS]
  ,entry  "they"    [dh.aa]
  ,entry  "they're" [dh.aa.r']
  ,entry  "thing"   [th.g']
  ,entry  "think"   [th.i.n'.k']
  ,entry  "this"    [dh.i.s']
  ,entry  "till"    [t.i.l']
  ,entryS "time"    [t.ii.m']
          [plural]
  ,entryS "tit"     [t.i.t']
          [plural]
  ,entry  "titular" [t.i.ch', l.r']
  ,entry  "to"      [t.ew]
  ,entry  "trace"   [tray.s']
  ,entry  "Tracy"   (tray ^: "see")
  ,entry  "tray"    [tray]
  ,entry  "true"    [t.r.ew]
  ,entry  "twitch"  [t.w.i.ch']] <>

  -- u section
  let up = u.p'
  in entries
  [entry  "under"    [u.n', d.r']
  ,entry  "until"    (n ^: "till")
  ,entry  "up"       [up]
  ,entry  "upcoming" (up ^: "come{^ing}")
  ,entry  "use"      [y.ew.s']] <>

  -- v section
  let val = v.a.l'
      ver = stks [v.er]
  in entries
  [entry  "val"      [val]
  ,entry  "value"    (val ^: "you")
  ,entry  "version"  [ver.shn']
  ,entry  "virgin"   [ver, j.n']
  ,entry  "Virginia" [v.r', j.n', y]] <>

  -- w section
  let well  = stks [w, e, l']
  in entries
  [entry  "want"    [w.aw.n'.t']
  ,entry  "was"     [w.u.z']
  ,entry  "we"      [w.ee]
  ,entry  "we've"   [w.ee.fvs']
  ,entry  "well"    [well]
  ,entryS "welcome" [well, come]
          [ing]
  ,entry  "were"    [w.er]
  ,entry  "what"    [w.u.t']
  ,entry  "where"   [w.e.r']
  ,entry  "which"   [w.i.ch']
  ,entry  "while"   [w.ii.l']
  ,entry  "who"     [h.ew]
  ,entry  "whore"   [h.o.r']
  ,entry  "will"    [w.i.l']
  ,entry  "witch"   [w.i, ch']
  ,entry  "with"    [w.i.dh']
  ,entry  "wood"    [w.oo, d']
  ,entry  "won"     [w.u.n']
  ,entryS "wonder"  ("won" +: d.r')
          [plural, ed, ing]
  ,entry  "would"   [w.oo.d']
  ,entry  "world"   [w.er.l'.d']
  ,entry  "worth"   [w.er.th']
  ,entry  "wren"    [w.r.e.n']
  ,entry  "wrench"  [w.r.e.n', ch']
  ,entry  "wretch"  [w.r.e.ch']
  ,entryS "write"   [wr.ii.t']
          [ing]
  ,entry  "written" [w.r.i.t'.n']
  ,entry  "wrote"   [r.oe.t']] <>

  -- x section
  entries
  [entry "x-ray" (x ^: "ray")] <>

  -- y section
  entries
  [entry  "you"  [y.ew]
  ,entry  "your" [y.o.r']] <>

  -- z section
  entries
  [entryS "zap" [z.a.p']
                [ed]]
