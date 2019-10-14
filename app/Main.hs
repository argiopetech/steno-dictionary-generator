module Main where

import CommonIssues
import Config
import ControlKeys
import Plover
import Primary
import Punctuation

import Dictionary

import Steno.Alphabet
import Steno.Numbers

import Control.Monad (when)


alphabetEntries = fingerspelling fingerspellingModifier capsModifier

numberEntries = hundreds hundredsModifier
             ++ reverses reverseModifier

specialKeyEntries = controlKeys controlModifier control (backspace : alphabet)
                 ++ controlKeys altModifier alt alphabet
                 ++ controlKeys superModifier super (space : alphabet)
                 ++ controlKeys controlRModifier controlR (home : alphabet)
                 ++ controlKeys (shiftModifier . superModifier)
                                (shift . super)
                                (enter : alphabet)
                 ++ controlKeys fingerspellingModifier id arrows
                 ++ controlKeys controlModifier control arrows
                 ++ controlKeys fingerspellingModifier id specialKeys

allEntries = alphabetEntries ++ numberEntries ++ specialKeyEntries ++ primaryDictionary ++ punctuation ++ coding ++ plover


main :: IO ()
main = do
  someEmpties <- printIfEmpty    $ checkEmpty     allEntries
  someDupls   <- printDuplicates $ checkDuplicate allEntries
  someBoundaryErrors <- printBoundaryErrors $ checkBoundaryErrors allEntries

  when (not (someEmpties || someDupls {-|| someBoundaryErrors-})) $ do
    writeJson "alphabet"     alphabetEntries
    writeJson "control-keys" specialKeyEntries
    writeJson "numbers"      numberEntries
    writeJson "primary"      primaryDictionary
    writeJson "punctuation"  punctuation
    writeJson "coding"       coding
    writeJson "plover"       plover

  where writeJson n = writeFile (n ++ ".json") . toJson
