module Main where

import CommonIssues
import Config
import ControlKeys
import Dotterel
import Plover
import Primary
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

specialKeyEntries =
  let specialLeft = (Entry "Left" [leftArrow <> stk Hash] :) $ tail arrows
      fSlash      = Entry "/" [stk O, stk E, stk U]
      cKeys       = space : tab : fSlash : backspace : alphabet
  in controlKeys controlModifier control cKeys
  ++ controlKeys (shiftModifier . controlModifier)
                 (shift . control)
                 cKeys
  ++ controlKeys altModifier alt alphabet
  ++ controlKeys superModifier super (tab : space : alphabet)
  ++ controlKeys controlRModifier controlR (home : alphabet)
  ++ controlKeys (shiftModifier . superModifier)
                 (shift . super)
                 (tab : enter : alphabet)
  ++ controlKeys fingerspellingModifier id arrows
  ++ controlKeys shiftModifier shift specialLeft
  ++ controlKeys controlModifier control arrows
  ++ controlKeys (shiftModifier . controlModifier)
                 (shift . control)
                 specialLeft
  ++ controlKeys fingerspellingModifier id specialKeys

allEntries = alphabetEntries ++ numberEntries ++ specialKeyEntries ++ primaryDictionary ++ punctuation ++ coding ++ plover ++ xmonad


main :: IO ()
main = do
  someBoundaryErrors <- printBoundaryErrors $ checkBoundaryErrors allEntries
  someEmpties <- printIfEmpty    $ checkEmpty     allEntries
  someDupls   <- printDuplicates $ checkDuplicate allEntries

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
    writeJson "dotterel"     dotterel

  where writeJson n = writeFile (n ++ ".json") . toJson
