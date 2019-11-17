{-# LANGUAGE RankNTypes #-}
module Primary where

import qualified Prelude as P
import Prelude ((<>), ($), String)
import Data.List (nub)

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
  let selection = P.map stroke $ P.filter ((P.==) w P.. name) primaryDictionary
      len = P.length selection
  in if len P.== 1
       then P.head selection
       else P.error $ P.show len <> " entries found for \"" <> w <> "\""

(++) :: String -> String -> [Stroke]
(++) w1 w2 = sstk w1 <> sstk w2

(+) :: String -> Stroke -> [Stroke]
(+) = go P.. sstk
  where go  [] s = [s]
        go [a] s = [a . s]
        go  ls s = P.init ls <> [P.last ls . s]
infixr 5 +

(+:) :: String -> Stroke -> [Stroke]
(+:) w1 s = sstk w1 <> [s]
infixr 5 +:

(^) :: Stroke -> String -> [Stroke]
(^) = (P.. sstk) P.. go
  where go s  [] = [s]
        go s [a] = [s . a]
        go s  ls = s . P.head ls : P.init ls
infixl 5 :

(^:) :: Stroke -> String -> [Stroke]
(^:) s w1 = s : sstk w1
infixl 7 ^:

nounSuffixes = [plural, contractS, pluralPosessive]
verbSuffixes = [ed, plural, ing]
pnoun  = grammar [contractS] -- Proper nouns
pnoun' = grammar [contractS, ed] -- Proper nouns
noun   = grammar nounSuffixes
noun'  = grammar (ed : nounSuffixes)
noun_  = \w s e -> grammar (nounSuffixes <> e) w s
verb   = grammar verbSuffixes
gverb  = grammar $ verbSuffixes <> P.map (combine ing) nounSuffixes
nvpair = grammar $ verbSuffixes <> P.tail nounSuffixes
ngv    = grammar $ verbSuffixes <> P.tail nounSuffixes <> P.map (combine ing) nounSuffixes

grammar ss w s = entryS w s ss


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
      ntenn = [n, t.e.n']
  in entries
  [entry  "a"         [aa]
  ,pnoun  "Aaron"     [e.r'.n']
  ,entry  "able"      [aa.b'.l']
  ,entry  "about"     [b.ow.t']
  ,entry  "ace"       [aa.s']
  ,entry  "actually"  [a.k', ch.ew.ly']
  ,entry  "add"       [a.d']
  ,noun   "addend"    ("add" ++ "end")
  ,entryS "addenda"   [d.e.n.d']       -- Irregular noun, addendum/addenda
          [contractS]
  ,entryS "addendum"  [d.e.n.d', m']   -- Irregular plural of "addendum"
          [contractS]
  ,entry  "after"     [after]
  ,noun   "afternoon" (after ^: "noon")
  ,entry  "against"   [g.e.n'.s'.t']
  ,verb   "agree"     [agree]
  ,noun   "agreement" [agree, m.n'.t']
  ,entryS "alga"      [a.l', j]        -- Irregular noun, alga/algae
          [contractS]
  ,entryS "algae"     [a.l', j.ee]     -- Irregular plural of "alga"
          [contractS]
  ,entry  "all"       [aw.l']
  ,noun   "allowance" [l.ow.n'.s']
  ,entry  "almost"    [aw.l', m.oe.st']
  ,entryS "alumna"    [l.u.m', n]      -- Irregular noun, alumna/alumnae
          [contractS]
  ,entryS "alumnae"   [l.u.m', n.aa]   -- Irregular plural of "alumna"
          [contractS]
  ,entryS "alumni"    [l.u.m', n.ii]   -- Irregular plural of "alumnus"
          [contractS]
  ,entryS "alumnus"   [l.u.m', n.s']   -- Irregular noun, alumnus/alumni
          [finalApostrophe]
  ,entry  "among"     [m.u.ng']
  ,nvpair "amount"    [m, ow.n'.t'] -- m',?
  ,entry  "an"        [a.n']
  ,entryS "analyses"  (n.a.l' ^: "sees") -- Irregular plural of "analysis"
          [finalApostrophe]
  ,entryS "analysis"  (n.a.l' ^: "sis")  -- Irregular noun, analysis/analyses
          [contractS]
  ,entry  "and"       [nd']
  ,nvpair "anger"     [g.r.r']
  ,verb   "announce"  [n, ow.n'.s'] -- n'?
  ,entry  "another"   [n.u.dh'.r']
  ,noun   "antenna"   ntenn             -- Sometimes irregular noun, antenna/antennae
  ,entryS "antennae"  (ntenn <> [aa])   -- Irregular plural of "antenna"
          [contractS]
  ,entry  "any"       [any]
  ,entryS "anybody"   ("any" ++ "body")
          [contractS]
  ,entry  "anything"  ("any" ++ "thing")
  ,entry  "anyhow"    ("any" ++ "how")
  ,entry  "anyone"    ("any" ++ "won")
  ,entry  "anywhere"  ("any" ++ "where")
  ,noun   "ape"       [aa.p']
  ,pnoun  "April"     [aa, p.r.l']
  ,noun   "apparatus" [a.p', r.a.t'.s'] -- Irregular noun, apparatus/apparatuses (handled appropriately by Plover)
  ,entryS "appendices" (sstk "pen" <> d.i ^: "sees") -- Irregular plural form of "appendix"
          [finalApostrophe]
  ,noun   "appendix"   ("pen" ++ "dick{^s}")         -- Irregular noun, appendix/appendices
  ,entryS "apparent"  [p.e.r'.n'.t']
          [ly]
  ,entry  "are"       [a.r']
  ,nvpair "arm"       [a.r'.m']
  ,entry  "as"        [a.z']
  ,noun   "ash"       [a.sh']
  ,entry  "ask"       [a.fvs'.k']
  ,entry  "at"        [a.t']
  ,entry  "available" [v.aa.l', b'.l']
--  ,entry  "available" [v.aa.b'.l']   -- Potential brief
  ,nvpair "average"   [a.fvs'.r'.g']
  ,entry  "awful"     [aw.fvs'.l']
  ,noun   "axe"       [a.x']
  ,entryS "axes"      ("axe" +: ee.s') -- Irregular plural of "axis"
          [finalApostrophe]
  ,entryS "axis"      ("axe" +: s)     -- Irregular noun, axis/axes
          [contractS]
  ,pnoun  "Azure"     [a.z', r']] <>

  -- b section
  let br = b.r
  in entries
  [entryS "bacilli"   [b, s.i.l', ii] -- Irregular plural of "bacillus"
          [contractS]
  ,entryS "bacillus"  [b, s.i.l', s'] -- Irregular noun, bacillus/bacilli
          [contractS]
  ,entryS "bacteria"  [b.k', t.ee.r']     -- Irregular noun, bacteria/bacterium
          [contractS]
  ,entryS "bacterium" [b.k', t.ee.r', m'] -- Irregular plural of "bacteria"
          [contractS]
  ,entry  "balm"      [b.aw.m']
  ,entry  "balmy"     ("balm" + ee)
  ,noun   "base"      [b.aa, s']
  ,entryS "bases"     [b.aa, s.ee.s'] -- Irregular plural of "basis"
          [finalApostrophe]
  ,entryS "basis"     [b.aa, s.s']    -- Irregular noun, basis/bases
          [contractS]
  ,entry  "be"        [b.ee]
  ,entryS "beau"      [b.ow.star]   -- Irregular noun, beau/beaux
          [contractS]
  ,entryS "beaux"     ("beau" + s') -- Irregular plural of "beau"
          [contractS]
  ,entry  "became"    (b ^: "came")
  ,entry  "because"   (b ^: "cause")
  ,entryS "become"    (b ^: "come")
          [plural]
  ,entry  "been"      [b.i.n']
  ,entry  "before"    [b, f.oe.r'] 
  ,entry  "began"     [b, g.a.n']  -- irregular past form of "begin"
  ,entryS "begin"     [b, g.i.n']  -- irregular verb begin/began/begun
          [ing]
  ,entry  "begun"     (b ^: "gun") -- irregular past-participle of "begin"
  ,entry  "beige"     [b.aa.zh']
  ,noun   "being"     [b.ee.ng']
  ,gverb  "belong"    [b, l.o.ng']
  ,entry  "bento"     [b.i.n', t.oe]
  ,entry  "best"      [b.e.st']
  ,entry  "bimodal"   [b.ii, m.oe, d.l']
  ,entryS "bin"       [b.i, n']
          [ed]
  ,nvpair "birth"     [b.er.th']
  ,entryS "bison"     ("buy" ++ "son") -- Irregular noun, bison/bison
          [contractS]
  ,noun   "bitch"     [b.i.ch']
  ,verb   "blame"     [b.l.aa.m']
  ,verb   "bleach"    [b.l.ee.ch']
  ,nvpair "blite"     [b.l.ii, t']
  ,entry  "blithe"    [b.l.ii.dh']
  ,entry  "blush"     [b.l.u.sh']
  ,nvpair "board"     [b.o.r', d']
  ,nvpair "body"      [b.o.d', ee]
  ,nvpair "bomb"      [b.aw.m', b']
  ,entry  "bored"     [b.o.r'.d']
  ,entry  "botch"     [b.o.ch']
  ,entry  "bottom"    [b.o.t'.m']
  ,nvpair "bow"       [b.ow]
  ,noun   "bow"       [b.oe]
  ,entry  "bread"     [b.r.e.d']
  ,entryS "break"     [br.aa.k']
          [ing]
  ,verb   "breathe"   [br.ee.th']
  ,entry  "breath"    [br.e.th']
  ,entry  "bring"     [br.g']
  ,entry  "broadcast" [br.o.d', k.a.st']
  ,entry  "bruise"    [br.ew.s']
  ,entry  "brush"     [br.u.sh']
  ,entry  "brutal"    [br.ew.t'.l']
  ,entry  "brutality" ("brutal" +: ty')
  ,noun   "budget"    [b.u.j'.t']
  ,entryS "buffalo"   ("buff" ++ "low") -- Irregular noun, buffalo/buffaloes
          [contractS]
  ,entryS "buffaloes" ("buffalo" + s')  -- Irregular plural form of "buffalo"
          [finalApostrophe]
  ,entry  "buff"      [b.u.fvs']
  ,noun   "build"     [b.i.l'.d']   -- irregular verb, build/built/built
  ,noun   "building"  ("build" +: g')
  ,entry  "built"     [b.i.l'.t']   -- irregular past form of "build"
  ,entryS "bureau"    [b, y.oo.r']    -- irregular noun bureau/bureaus
          [contractS]
  ,entryS "bureaus"   ("bureau" + s') -- irregulat plural form of "bureau"
          [finalApostrophe]
  ,entryS "bus"       [b.u.s']      -- irregular nvpair "bus"/"buses", "bus"/"bussed"/"bussed"
          [ed, ing]
  ,entry  "buses"     ("bus" +: s') -- irregular plural/past form of "bus"
  ,nvpair "bush"      [b.oo.sh]
  ,entry  "but"       [b.u.t']
  ,noun   "butter"    [b.u.t', r']
  ,noun   "buy"       [b.ii]
  ,noun   "buyer"     ("buy" + r')
  ,entry  "by"        [b.i]] <>

  -- c section
  let cert  = s.er.t'
      con   = k.o.n'
      cr    = k.r
  in entries
  [entry  "cactii"    (k.a.k' ^: "tie") -- irregular plural form of "cactii"
  ,entry  "cactus"    [k.a.k', t.u.s']  -- irregular noun cacus/cactii
  ,noun   "cab"       [k.a.b']
  ,noun   "cad"       [k.a.d']
  ,pnoun  "caffeine"  ("calf" +: ee.n')
  ,entry  "calf"      [k.a.fvs']    -- irregular noun calf/calves
  ,nvpair "call"      [k.aw.l']
  ,verb   "calm"      [k.aw, m'] 
  ,entry  "calves"    ("calf" + s') -- irregular plural form of "calf"
  ,entry  "came"      [k.aa.m']  -- irregular past form of "come"
  ,nvpair "can"       [k.a.n']
  ,noun   "car"       [k.aw.r']
  ,noun   "carriage"  [k.a.r'.j']
  ,pnoun  "Carly"     ("car" +: ly')
  ,nvpair "castle"    [k.a.fvs'.l']
  ,noun   "cat"       [k.a.t']
  ,entry  "catch"     [k.a.ch']   -- irregular verb, catch/caught/caught
  ,entry  "caught"    [k.aw.t']   -- irregular past form of "catch"
  ,nvpair "cause"     [k.aw.z']
  ,entry  "cd"        [k.r.d']    -- as in, fingerspelling 'c'
  ,verb   "cease"     ("see" +: s')
  ,nvpair "cement"    [s.m.e.n'.t']
  ,noun   "cert"      [cert]
  ,noun   "certification" [cert, f.k', aa.sh'.n']
  ,entry  "chaff"     [ch.a.fvs']
  ,entry  "chance"    [ch.a.n'.s']
  ,entry  "change"    [ch.aa.n'.g']
  ,noun   "channel"   [ch.a.n'.l']
  ,nvpair "chant"     [ch.a.n'.t']
  ,nvpair "charm"     [ch.a.r'.m']
  ,nvpair "chart"     [ch.a.r'.t']
  ,noun   "chasm"     [k.a.fvs'.m']
  ,nvpair "chat"      [ch.a.t']
  ,nvpair "check"     [ch.e.k']
  ,entry  "chess"     [ch.e.s']
  ,noun   "chest"     [ch.e.st']
  ,entry  "child"     [ch.ii.l'.d']
  ,entry  "children"  ("chill{^ed}" +: r.n')
  ,verb   "chill"     (ch ^ "ill")
  ,noun   "chin"      [ch.i.n']
  ,entry  "chive"     [ch.ii.fvs']
  ,entry  "choose"    [ch.oo.s']   -- irregular verb choose/chose/chosen
  ,verb   "chop"      [ch.o.p']
  ,entry  "chore"     [ch.oe.r']
  ,entry  "chose"     [ch.oe.s']   -- irregular past form of "choose"
  ,entry  "chosen"    [ch.oe.s', n'] -- irregular past participle of "choose"
  ,entryS "chrome"    (k ^ "Rome")
          [ed]
  ,verb   "chuck"     [ch.u.k']
  ,nvpair "churn"     [ch.er.n']
  ,ngv    "coat"      [k.oe.t']
  ,noun   "cod"       [k.o.d']
  ,nvpair "code"      [k.oe.d']
  ,nvpair "cog"       [k.o.g']
  ,nvpair "coil"      [k.oi.l']
  ,entryS "come"      [k.o.m']    -- irregular verb come/came/come
          [ing]
  ,verb   "commend"   [k.m.e.n'.d']
  ,nvpair "commit"    [k.m.i.t']
  ,noun   "con"       [con]
  ,nvpair "conflict"  [con, f.l.i.k'.t']
  ,nvpair "continue"  [k.n', t.i.n', y]
  ,noun   "contract"  [con, t.r.a.k'.t']
  ,verb   "contract"  [k.n', t.r.a.k'.t']
  ,nvpair "cock"      [k.aw.k']
  ,entry  "coke"      [k.oe.k']
  ,entry  "cop"       [k.o.p']
  ,entry  "cope"      [k.oe.p']
  ,entryS "copy"      [k.aw.p'.ee]
          [plural]
  ,entry  "core"    [k.oe.r']
  ,entry  "corps"   [k.oe, r'] -- irregular noun corps/corps
  ,entry  "corpora" ("core" +: p.r')
  ,noun   "corporation" (sstk "core" <> [p.r', aa.sh'.n'])
  ,entry  "corpus"  ("core" ++ "pus") -- irregular noun corpus/corpora
  ,verb   "correct" [k.r', e.k'.t']
  ,verb   "corrupt" [k.r, u.p'.t']
  ,entry  "cot"     [c.o.t']
  ,entry  "could"   [k.oo.d']
  ,noun   "course"  [k.o.r', s']
  ,entry  "cover"   [k.o.fvs'.r']
  ,entry  "crap"    [cr.a.p']
  ,entry  "crash"   [cr.a.sh']
  ,entry  "crawl"   [cr.aw.l']
  ,entry  "cred"    [cr.e.d']
  ,entryS "credit"  [cr.e.d', t']
          [ed]
  ,nvpair "crew"    [cr.ew]
  ,entry  "cringe"  [cr.i.n, j']
  ,entryS "cross"   [cr.aw.s']
          [ed]
  ,entryS "crow"    (k ^ "row")
          [ed, ing]
  ,entry  "crude"   [cr.ew, d']
  ,entry  "cruel"   [cr.ew.l']
  ,entry  "cruise"  [cr.ew, z']
  ,entryS "crush"   [cr.u.sh']
          [ed, ing]
  ,entry  "crutch"  [cr.u.ch']
  ,noun   "crystal" [k.r.i.fvs'.t'.l']
  ,noun   "cub"     [k.u.b']
  ,noun   "cube"    [k, y.ew.b']
  ,entry  "cuff"    [k.u.fvs']
  ,entry  "cull"    [k.u.l']
  ,noun   "cunt"    [k.u.n'.t']
  ,noun   "cup"     [k.u.p']
  ,verb   "cure"    [k, y.oo.r']
  ,verb   "curse"   [k.er.s']
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
  ,noun   "dick"  [d.i.k']
  ,entry  "diff"   [d.i.fvs']
  ,entry  "different" [d.i.fvs'.r'.n'.t']

  ,entry  "digit" [d.i.j'.t']
  ,entry  "din"   [d.i.n']
  ,entry  "dine"  [d.ii.n']
  ,entry  "diner" [d.ii.n', r']
  ,entry  "dinner" [d.i.n', r']
  ,verb   "distill" (d ^: "still")
  ,entry  "ditch" [d.i.ch']
  ,entry  "do"     [d.ew]
  ,entry  "don't"  [d.oe.n'.t']
  ,entry  "does"   [d.u.z']
  ,entry  "doesn't" [d.u.z', n'.t']
  ,entry  "done"   [d.u.n']
  ,noun   "donor" [d.oe.n', r']
  ,verb   "double" [d.u.b'.l']
  ,verb   "drag"  [d.r.a.g']
  ,entry  "drain"  [drain]
  ,nvpair "dream" [d.r.ee.m']
  ,nvpair "drug" [d.r.u.g']
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
  ,entryS "eat"    [ee.t'] -- irregular verb, eat/ate/eaten
          [ing]
  ,nvpair "echo"      [e.k']
  ,nvpair "edge"      [e.j']
  ,nvpair "edit"      [e.d',t']
  ,noun   "eel"       [ee.l']
  ,noun   "effort"    [f.r'.t']
  ,pnoun  "Elliot"    [e.l'.t']
  ,verb   "eke"       [ee.k']
  ,entry  "elf"       [l.fvs']    -- Has irregular plural
  ,entry  "else"      [e.l'.s']
  ,entry  "elves"     [l.fvs'.z'] -- Irregular plural of "elf"
  ,entryS "email"     (ee ^: "mail")
          [ing]
  ,entry  "emacs"     [ee, m.a.x']
  ,noun   "emergency" ("merge" +: n.s'.ee)
  ,pnoun  "Emily"     [e.m, ly']
  ,verb   "enact"     [n.a.k'.t']
  ,nvpair "end"       [n, d']
  ,noun   "entry"     [n, t.r.ee]
  ,gverb  "etch"      [e.ch']
  ,entry  "even"      [ee.fvs'.n']
  ,entry  "ever"      [e.fvs'.r']
  ,entry  "every"     ("ever" +: ee)
  ,entry  "evil"      [ee.fvs'.l']
  ,entry  "euro"      [y.oo.r']
  ,noun   "exam"      [x.a.m']
  ,verb   "examine"   ("exam" +: n')
  ,noun   "example"   ("exam" +: p'.l')
  ,entry  "except"    [x.e.p'.t']] <>

  -- f section
  let fee = f.ee
  in entries
  [entry  "fable"   [f.aa.bl']
  ,entryS "faculty" [f.a.k'.l'.ty']
                    [contractS]
  ,entry  "false"   [f.aw.l'.s']
  ,noun   "fang"    [f.aa.ng']
  ,noun   "fee"     [fee]
  ,entry  "feeble"  [fee.bl']
  ,entry  "feral"   [f.e.r'.l']
  ,verb   "fetch"   [f.e.ch']
  ,verb   "fidget"  [f.i.j'.t']
  ,entry  "fierce"  [f.ee.r'.s']
  ,entry  "fifth"   [f.i.fvs'.th']
  ,entry  "find"    [f.ii.nd']      -- irregular verb, find/found/found
  ,entry  "finger"  [f.g', r']
  ,entry  "fire"    [f.ii.r']
  ,entry  "fish"    [f.i.sh']
  ,entry  "flash"   [f.l.a.sh']
  ,entry  "flaunt" [f.l.aw.n'.t']
  ,entry  "flesh" [f.l.e.sh']
  ,entry  "float"   [f.l.oe.t']
  ,entry  "flung" [f.l.u.ng']
  ,entry  "flush" [f.l.u.sh']
  ,entry  "for"     [f.o.r']
  ,entry  "ford" [f.o.r'.d']
  ,entry  "forge" [f.o.r'.j']
  ,entryS "form" [f.o.r'.m'] [plural]
  ,entry  "found"    [f.ow.n'.d']  -- irregular plural form of "find"
  ,entry  "free"    [f.r.ee]
  ,entry  "fresh"   [f.r.e.sh']
  ,noun_  "friend"  [f.r.e.n'.d']
          [ly]
  ,entry  "fries"  [f.r.ii.z']
  ,entry  "frigid" [f.r.i.j'.d']
  ,entry  "fringe" [f.r.i.n, j']
  ,entry  "from"    [f.r.o.m']
  ,entry  "frost"   [f.r.o.st']
  ,entry  "fuck"    [f.u.k']] <>

  -- g section
  entries
  [noun   "gadget"    [g.a.j'.t']
  ,noun'  "gang"      [g.a.ng']
  ,noun   "garage"    [g.r.aw.zh']
  ,nvpair "gash"      [g.a.sh']
  ,entry  "gave"      [g.aa.fvs']       -- irregular past form of "give"
  ,noun   "germ"      [j.e.r'.m']
  ,noun   "gerund"    [j.e.r', n'.d']
  ,entry  "get"       [g.e.t']          -- Irregular verb, get/got/gotten
  ,entry  "gift"      [g.i.fvs'.t']
  ,pnoun  "git"       [g.i.t']
  ,entry  "give"      [g.i.fvs']        -- irregular verb, give/gave/given
  ,entry  "given"     [g.i.fvs'.n']     -- irregular past participle of "give"
  ,entry  "glad"      [g.l.a.d']
  ,verb   "glare"     [g.l.aa.r']
  ,verb   "glean"     [g.l.ee.n'.s']
  ,verb   "glide"     [g.l.ii.d']
  ,nvpair "glitch"    [g.l.i.ch']
  ,noun   "gnome"     [n.oe.m']
  ,noun   "goblet"    [g.o.b'.l'.t']
  ,entry  "good"      [g.oo.d']
  ,entry  "gosh"      [g.o.sh']
  ,entry  "got"       [g.o.t']
  ,entry  "gotten"    ("got" +: n')
  ,noun   "gram"      [g.r.a.m']
  ,noun   "grammar"   ("gram" +: r')
  ,nvpair "grant"     [g.r.a.n'.t']
  ,noun   "gravel"    [g.r.a.fvs'.l']
  ,entry  "great"     [g.r.aa.t']
  ,nvpair "group"     [g.r.ew.p']
  ,noun   "growth"    [g.r.oe.th']
  ,noun   "grudge"    [g.r.u.j']
  ,entry  "grunge"    [g.r.u.n', j']
  ,nvpair "grunt"     [g.r.u.n'.t']
  ,nvpair "guarantee" [g.e.r'.n'.ty']
  ,noun   "guest"     [g.e.st']
  ,nvpair "guide"     [g.ii.d']
  ,noun   "gun"       [g.u.n']
  ,noun   "gym"       [j.i.m']] <>

  -- h section
  let hand = stks [h.a.n'.d']
  in entries
  [entry  "had"         [h.a.d']
  ,noun   "hag"         [h.a.g']
  ,entry  "half"        [h.a, fvs'] -- irregular half/halves, half/halve/halved
  ,noun   "hall"        [h.aw.l']
  ,verb   "halt"        [h.aw.l'.t']
  ,nvpair "hand"        [hand]
  ,pnoun  "handwriting" (hand ^: "write{^ing}")
  ,verb   "harm"        [h.aw.r'.m']
  ,entry  "has"         [h.a.z']
  ,verb   "hash"        [h.a.sh']
  ,noun'  "hat"         [h.a.t']
  ,nvpair "hatch"       [h.a.ch']
  ,entry  "hath"        [h.a.th']
  ,entryS "have"        [h.a.fvs']
                        [ing]
  ,entry  "he"          [h.ee]
  ,nvpair "head"        [h.e.d']
  ,verb   "heal"        [h.ee.l']
  ,noun   "health"      [h.e.l'.th']
  ,nvpair "heap"        [h.ee.p']
  ,entry  "hear"        [h.ee.r']    -- irregular verb, hear/heard/heard
  ,nvpair "heat"        [h.ee.t']
  ,nvpair "heel"        [h.ee'.l']
  ,noun   "height"      [h.ii.t']
  ,noun   "hell"        [h.e.l']
  ,entry  "her"         [h.er]
  ,entry  "here"        [h.ee'.r']
  ,nvpair "hermit"      [h.er.m'.t']
  ,entry  "hid"         [h.i.d']      -- irregular plural of "hide"
  ,entry  "hidden"      ("hid" +: n') -- irregular past participle of "hide"
  ,entry  "hide"        [h.ii.d']     -- irregular verb, hide/hid/hidden
  ,nvpair "hinge"       [h.i.n, j']
  ,entry  "hit"         [h.i.t']      -- irregular verb hit/hit/hit
  ,entry  "hoary"       ("whore" + ry')
  ,pnoun  "hoarfrost"   ("whore" ++ "frost")
  ,nvpair "hog"         [h.o.g']
  ,noun'  "hood"        [h.oo.d']
  ,verb   "hoe"         [h.oe]
  ,verb   "hop"         [h.o.p']
  ,verb   "hope"        [h.oe.p']
  ,entry  "horrible"    ("whore" + bl')
  ,noun   "horror"      ("whore" +: r')
  ,nvpair "host"        [h.oe.st']
  ,entryS "hot"         [h.o.t']
          [plural]
  ,entry  "house"       [h.ow.s']
  ,entry  "hover"       [h.o.fvs'.r']
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
  ,entry  "ill"       [i.l']
  ,entry  "in"        [n]
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
  ,entry  "low"      [l.oe]
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
  ,entry  "meant"     [m.e.n'.t']
  ,entryS "meet"      [mee.t'] [ing]
  ,entry  "merge"     [m.er.j']
  ,entry  "mesh"      [m.e.sh']
  ,entry  "meth"      [m.e.th']
  ,entry  "method"    [m.e.th'.d']
  ,entry  "mic"       [m.ii, k']
  ,pnoun  "Microsoft" [m.s']
  ,pnoun  "Mike"      [m.ii.k']
  ,entry  "mint"      [m.i.n'.t']
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
  ,entry  "pen"      [p.e.n']
  ,entry  "pent"     ("pen" + t')
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
  ,entry  "pout"     ("pow" + t')
  ,entry  "pow"      [p.ow]
  ,entry  "power"    ("pow" + r')
  ,entry  "praise"   (p ^ "raise")
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
  ,nvpair "race" [r.aa.s']
  ,nvpair "rag" [r.a.g']
  ,nvpair "raid" [r.aa.d']
  ,ngv    "rail" [r.aa.l']
  ,verb   "raise" [r.aa.z']
  ,entry  "ran"  [r.a.n']   -- irregular past form of "run"
  ,entry  "rang" [r.aa.ng']
  ,nvpair "range" [r.aa.n', j']
  ,nvpair "rant" ("ran" + t')
  ,verb   "rap" [r.a.p']
  ,entry  "rape" [r.aa.p']
  ,entry  "rash" [r.a.sh']
  ,noun   "rat" [r.a.t']
  ,entry  "ratchet" [r.a.ch'.t']
  ,entry  "rate" [r.aa.t']
  ,entry  "raw" [r.aw]
  ,entry  "ray" [r.aa]
  ,entry  "read" [r.ee'.d']
  ,entry  "real"    [r.ee'.l']
  ,nvpair "reason"  [r.ee.fvs'.n']
  ,entry  "rebel" [r.e.b'.l']
  ,entry  "rebel" [r, b.e.l']
  ,verb "receive" [r, s.ee.fvs']
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
  ,entry  "rid"   [r.i.d']
  ,entry  "rifle" [r.ii.fvs'.l']
  ,entry  "rift"  [r.i.fvs'.t']
  ,entry  "rig"   [r.i.g']
  ,entry  "right" [r.ii.t']
  ,entry  "ripe"  [r.ii.p']
  ,entry  "river" [r.ii.fvs'.r']
  ,entry  "road"   ("row" +: d')
  ,entry  "roar"   [r.oe.r']
  ,entryS "rob"    [r.o.b'] [ed]
  ,entry  "robe"   ("row" + b')
  ,entryS "rocket" [r.o.k'.t'] [plural]
  ,entry  "rod"    [r.o.d']
  ,entry  "rode"   ("roe" + d')
  ,entry  "roe"    ("row" + star)
  ,entry  "rog"    [r.o.j']
  ,entry  "roll"   ("row" + l')
  ,pnoun  "Rome"   ("row" + m')
  ,entry  "rope"   ("row" + p')
  ,entry  "rot"    [r.o.t']
  ,entry  "rote"   ("row" + t')
  ,entry  "rough"  [r.u.fvs']
  ,entry  "rove"   [r.oe.fvs']
  ,entry  "row"    [r.oe]
  ,verb   "rub"    [r.u.b']
  ,entry  "rubble" [r.u.b'.l']
  ,entry  "rude"   [r.ew.d']
  ,verb   "ruffle" [r.u.fvs'.l']
  ,noun   "rug"    [r.u.g']
  ,entry  "rugged" ("rug" + d')
  ,noun   "rule"   [r.ew.l']
  ,pnoun  "rum"    [r.u.m']
  ,entryS "run"    [r.u.n']   -- irregular verb run/ran/run
          [ing]
  ,entry  "rung"   [r.u.ng']
  ,entry  "ruse"   [r.ew.z']
  ,entry  "rush"   [r.u.sh']
  ,entry  "rut"    [r.u.t']] <>

  -- s section
  let see  = s.ee
      sis  = s.i.s'
      some = s.u.m'
      spun = s.p.u.n'
      stew = s.t.ew
      sure = s.oo.r'
  in entries
  [entry  "sad"      [s.a.d']
  ,verb   "sag"      [s.a.g']
  ,entry  "said"     [s.e.d']
  ,nvpair "sail"     [s.aa, l']
  ,entry  "sale"     [s.aa.l']
  ,entry  "sap"      [s.a.p']
  ,pnoun  "Sasha"    [s.aw.sh']
  ,entry  "sass"     [s.a.s']
  ,entry  "sat"      [s.a.t']
  ,entry  "Saul"     [s.aw.l']
  ,verb   "saw"      [s.aw]       -- irregular past form  of "see"
  ,entry  "says"     [s.e.z']
  ,entry  "scaffold" [s.k.a.fvs'.l'.d']
  ,entry  "scald"    [s.aw.l'.d']
  ,entry  "scar"     [s.k.a.r']
  ,entry  "scarlet"  ("scar" + l'.t')
  ,entry  "school"   [s.k.ew.l']
  ,entry  "scoff"    [s.k.aw.fvs']
  ,entry  "scour"    [s.k.ow.r']
  ,entry  "scribble" [s.k.r.i.b'.l']
  ,entry  "scribe"   [s.k.r.ii.b']
  ,entryS "scuffle"  [s.k.u.fvs'.l'] [ed]
  ,entry  "sea"      [s.ee']
  ,entry  "seat"     ("see" + t')
  ,noun   "secret"   ("see" +: k.r'.t')
  ,verb   "secrete"  [s, k.r.ee.t']
  ,entry  "see"      [see]        -- irregular verb see/saw/seen
  ,entry  "seed"     ("see" + d')   
  ,entry  "seen"     ("see" + n') -- irregular past participle of "see"
  ,entry  "sees"     ("see" + s')
  ,entry  "seethe"   ("see" + th')
  ,entry  "seize"    ("see" + z')
  ,entry  "sell"     [s.e.l']
  ,noun   "semester" [s, m.e.st', r']
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
  ,entry  "sis"      [s.i.s']
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
  ,verb   "soar"     [s.oe, r']
  ,nvpair "sob"      [s.aw.b']
  ,nvpair "soil"     [s.oi.l']
  ,entry  "sold"     [s.oe.l'.d']
  ,entry  "some"     [some]
  ,entryS "somebody"  ("some" ++ "body")
          [contractS]
  ,entry  "somehow"   ("some" ++ "how")
  ,entryS "someone"   ("some" ++ "won")
          [contractS]
  ,entry  "someplace" ("some" ++ "place")
  ,entryS "something" ("some" ++ "thing")
          [contractS]
  ,entry  "sometime"  ("some" ++ "time")
  ,entry  "sometimes" ("some" ++ "time{^s}")
  ,entry  "somewhere" ("some" ++ "where")
  ,noun   "son"       [rep [O] $ P.head $ sstk "sun"]
  ,noun   "song"      [s.o.ng']
  ,pnoun  "soot"      [s.oo.t']
  ,verb   "sop"       [s.o.p']
  ,entry  "sore"      [s.oe.r']
  ,noun   "sound"     [s.ow.nd']
  ,entry  "soy"       [s.oi]
  ,verb   "spew"      (s.p ^: "you")
  ,noun   "sphere"    [s.f.ee.r']
  ,nvpair "spill"     [s.p.i.l']
  ,entry  "spin"      [s.p.i.n']  -- irregular verb spin/spun/spun
  ,nvpair "spite"     [s.p.ii.t']
  ,nvpair "splash"    [s.p.l.a.sh']
  ,entry  "splotch"   [s.p.l.o.ch']
  ,entry  "sponge"    [spun, j']
  ,noun   "spore"     [s.p.oe.r']
  ,nvpair "spot"      [s.p.o.t']
  ,noun   "spouse"    [s.p.ow.s']
  ,verb   "sprawl"    [s.p.r.aw.l']
  ,entry  "spray"     [s.p.r.aa]
  ,nvpair "sprout"    [s.p.r.ow.t']
  ,entry  "spun"      [spun]        -- irregular post form of "spin"
  ,nvpair "squash"    [s.q.a.sh']
  ,verb   "squish"    [s.q.i.sh']
  ,entry  "stack"     [s.t.a.k']
  ,entry  "stair"     [s.t.aa, r']
  ,entry  "star"      [s.t.a.r']
  ,entry  "stare"     [s.t.aa.r']
  ,entryS "start"     [s.t.a.r'.t']
          [ed]
  ,entry  "stash"     [s.t.a.sh']
  ,entryS "stat"      [s.t.a.t']
          [plural]
  ,noun   "statistics" [s.t.a, t.i.s', t.i.x']
  ,entry  "status"     [s.t.a, t.s']
  ,nvpair "stew"       [stew]
  ,noun   "steward"    ("stew" + r'.d')
  ,pnoun  "Stewart"    ("stew" +: r'.t')
  ,entry  "still"      (s ^ "till")
  ,verb   "store"      [s.t.o.r']
  ,entry  "storage"    [s.t.o.r'.j']
  ,nvpair "strap"      [s.t.r.a.p']
  ,entry  "straw"      [s.t.r.aw]
  ,entry  "stress"     [s.t.r.e.s']
  ,entry  "stretch"    [s.t.r.e.ch']
  ,entryS "string"     [s.t.r.g']
                       [plural]
  ,entry  "strong"     [s.t.r.aw.ng']
  ,entry  "stronger"   [s.t.r.aw.ng', r']
  ,pnoun  "Stuart"     ("stew" + r'.t')
  ,noun   "student"    ("stew" +: dnt)
  ,verb   "stun"       [s.t.u.n']
  ,entry  "stung"      ("stun" + g')  -- irregular past form of "sting"
  ,entry  "success"    [s.k', s.e.s']
  ,entry  "such"       [s.u.ch']
  ,noun   "sugar"      [s.h.oo.g',r']
  ,noun   "sun"        [s.u.n']
  ,entry  "superb"     [s.p.er.b']
  ,entry  "suppress"   [s.p.r.e.s']
  ,entryS "sure"       [sure]
          [ly]
  ,entryS "surround"   [s.r.ow.n'.d']
          [plural]
  ,nvpair "switch"     [s.w.i.ch']
  ,nvpair "syringe"    [s.r.i.n', j']
  ,noun   "syrup"      [s.er.p']
  ,noun   "system"     [sis, t.m']] <>

  -- t section
  let thank = stks [th, a, ng', k']
  in entries
  [nvpair "tab"     [t.a.b']
  ,noun   "teat"    [t.ee.t]
  ,nvpair "test"    [t.e.fvs'.t']
  ,nvpair "text"    [t.e.x'.t']
  ,verb   "thank"   [thank]
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
  ,nvpair "time"    ("tie" + m')
  ,nvpair "tie"     [t.ii]
  ,noun   "tit"     [t.i.t']
  ,entry  "titular" [t.i.ch', l.r']
  ,entry  "to"      [t.ew]
  ,nvpair "trace"   ("tray" + s')
  ,pnoun  "Tracy"   ("tray" ++ "see")
  ,entry  "tray"    [t.r.aa]
  ,entry  "true"    [t.r.ew]
  ,nvpair "twitch"  [t.w.i.ch']] <>

  -- u section
  let up = u.p'
  in entries
  [entry  "under"    [u.n', d.r']
  ,entry  "until"    (n ^: "till")
  ,entry  "up"       [up]
  ,entry  "upcoming" (up ^: "come{^ing}")
  ,verb   "use"      [y.ew.s']] <>

  -- v section
  let val = v.a.l'
      ver = stks [v.er]
  in entries
  [entry  "val"      [val]
  ,entry  "value"    (val ^: "you")
  ,noun   "version"  [ver.shn']
  ,noun   "virgin"   [ver, j.n']
  ,pnoun  "Virginia" [v.r', j.n', y]] <>

  -- w section
  let well  = stks [w, e, l']
  in entries
  [verb   "want"    [w.aw.n'.t']
  ,entry  "was"     [w.u.z']
  ,entryS "we"      [w.ee]
          [contractLL, contractD]
  ,entry  "we've"   [w.ee.fvs']
  ,entry  "well"    [well]
  ,nvpair "welcome" ("well" ++ "come")
  ,entry  "were"    [w.er]
  ,entry  "what"    [w.u.t']
  ,entry  "where"   [w.e.r']
  ,entry  "which"   [w.i.ch']
  ,entry  "while"   [w.ii.l']
  ,entry  "who"     [h.ew]
  ,noun   "whore"   [h.o.r']
  ,entry  "will"    [w.i.l']
  ,noun   "witch"   [w.i, ch']
  ,entry  "with"    [w.i.dh']
  ,noun   "wood"    [w.oo, d']
  ,entry  "won"     [w.u.n']
  ,verb   "wonder"  ("won" +: d.r')
  ,entry  "would"   [w.oo.d']
  ,noun   "world"   [w.er.l'.d']
  ,entry  "worth"   [w.er.th']
  ,noun   "wren"    [w.r.e.n']
  ,nvpair "wrench"  [w.r.e.n', ch']
  ,noun   "wretch"  [w.r.e.ch']
  ,entryS "write"   [wr.ii.t']    -- Irregular verb write/wrote/written
          [ing]
  ,entry  "written" [w.r.i.t'.n'] -- Irregular verb write/wrote/written
  ,entry  "wrote"   [w.r.oe.t']     -- Irregular verb write/wrote/written
  ] <>

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
