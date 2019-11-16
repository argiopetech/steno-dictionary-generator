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

specialKeyEntries =
  let specialLeft = (Entry "Left" [leftArrow <> stk Hash] :)
                  $ home : end : tail arrows
      fSlash      = Entry "/" [stk O, stk E, stk U]
      cKeys       = dot : space : tab : fSlash : backspace : alphabet
  in controlKeys controlModifier control cKeys
  ++ controlKeys (shiftModifier . controlModifier)
                 (shift . control)
                 cKeys
  ++ controlKeys altModifier alt (fSlash : tab : alphabet)
  ++ controlKeys (shiftModifier . altModifier) (shift . alt) [tab]
  ++ controlKeys superModifier super (tab : space : alphabet)
  ++ controlKeys controlRModifier controlR (home : alphabet)
  ++ controlKeys (shiftModifier . superModifier)
                 (shift . super)
                 (space : tab : enter : alphabet)
  ++ controlKeys shiftModifier shift specialLeft
  ++ controlKeys controlModifier control arrows
  ++ controlKeys (shiftModifier . controlModifier)
                 (shift . control)
                 specialLeft
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
