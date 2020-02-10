module Main where

import CommonIssues
import Config
import ControlKeys
import CSharp
import Dotterel
import Haskell
import Plover
import Primary hiding ((.), (++))
import Punctuation
import Dictionary
import Emacs
import Keys
import Sounds
import Steno.Alphabet
import Steno.Numbers
import Stroke
import XMonad

import Control.Monad (when)


alphabetEntries = fingerspelling fingerspellingModifier capsModifier

numberEntries = hundreds hundredsModifier
             ++ reverses reverseModifier
             ++ doubles  doublesModifier
             ++ cardinals

tinyModifications = reverses (addVowel E)


-- This needs massive improvement
specialKeyEntries =
  let specialLeft = (Entry "Left" [leftArrow <> stk Hash] :)
                  $ home : end : tail arrows
      fSlash      = Entry "slash" [stk O, stk E, stk U]
      bSlash      = Entry "backslash" [s <> l <> a]
      dawt        = Entry "period"    [d <> o <> t']
      cKeys       = bSlash : enter : dawt : space : tab : fSlash : backspace : alphabet
  in controlKeys controlModifier control cKeys
  ++ controlKeys (shiftModifier . controlModifier)
                 (shift . control)
                 (cKeys ++ specialLeft)
  ++ controlKeys altModifier alt (enter : bSlash : fSlash : tab : dawt : alphabet)
  ++ controlKeys (shiftModifier . altModifier) (shift . alt) (enter : [tab])
  ++ controlKeys superModifier super (enter : tab : space : alphabet)
  ++ controlKeys controlRModifier controlR (home : alphabet)
  ++ controlKeys (shiftModifier . superModifier)
                 (shift . super)
                 (space : tab : enter : alphabet)
  ++ controlKeys shiftModifier shift (tab : specialLeft)
  ++ controlKeys controlModifier control arrows
  ++ controlKeys fingerspellingModifier id arrows
  ++ controlKeys fingerspellingModifier id specialKeys

allEntries = alphabetEntries ++ numberEntries ++ specialKeyEntries ++ primaryDictionary ++ punctuation ++ plover ++ xmonad

phoneEntries = dedupEntries $ alphabetEntries
                           ++ numberEntries
                           ++ punctuation
                           ++ primaryDictionary
                           ++ plover
                           ++ controlKeys fingerspellingModifier id
                                          (arrows ++ specialKeys)
                           ++ controlKeys controlModifier control
                                          alphabet
                           ++ dotterel


main :: IO ()
main = do
--  someBoundaryErrors <- printBoundaryErrors $
--                          checkBoundaryErrors . notEmpty $ allEntries
  someDupls   <- printDuplicates $ checkDuplicate . notEmpty $ allEntries
  someEmpties <- printIfEmpty    $ checkEmpty allEntries

  when (not (someEmpties || someDupls {-|| someBoundaryErrors-})) $ do
    writeJson "alphabet"     alphabetEntries
    writeJson "control-keys" specialKeyEntries
    writeJson "numbers"      numberEntries
    writeJson "primary"      primaryDictionary
    writeJson "punctuation"  punctuation
    writeJson "coding"       coding
    writeJson "plover"       plover
    writeJson "emacs"        emacs
    writeJson "xmonad"       xmonad
    writeJson "tinymod"      tinyModifications
    writeJson "dotterel"     phoneEntries
    writeJson "csharp"       csharp
    writeJson "haskell"      haskell

  where writeJson n = writeFile (n ++ ".json") . toJson
