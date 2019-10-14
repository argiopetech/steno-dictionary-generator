module Main where

import CommonIssues
import Config
import ControlKeys
import Primary

import Dictionary

import Steno.Alphabet
import Steno.Numbers

import Control.Monad (when)


alphabetEntries = fingerspelling fingerspellingModifier capsModifier

numberEntries = hundreds hundredsModifier
             ++ reverses reverseModifier

specialKeyEntries = controlKeys controlModifier control alphabet
                 ++ controlKeys altModifier alt alphabet
                 ++ controlKeys superModifier super (space : alphabet)
                 ++ controlKeys controlRModifier controlR (home : alphabet)
                 ++ controlKeys (shiftModifier . superModifier)
                                (shift . super)
                                (enter : alphabet)
                 ++ controlKeys fingerspellingModifier id arrows
                 ++ controlKeys controlModifier control arrows
                 ++ controlKeys fingerspellingModifier id specialKeys

allEntries = alphabetEntries ++ numberEntries ++ specialKeyEntries ++ primaryDictionary


main :: IO ()
main = do
  someEmpties <- printIfEmpty    $ checkEmpty     allEntries
  someDupls   <- printDuplicates $ checkDuplicate allEntries
  someBoundaryErrors <- printBoundaryErrors $ checkBoundaryErrors allEntries

  when (not (someEmpties || someDupls {-|| someBoundaryErrors-})) $ do
    writeJson "Alphabet"     alphabetEntries
    writeJson "Control-Keys" specialKeyEntries
    writeJson "Numbers"      numberEntries
    writeJson "Primary"      primaryDictionary

  where writeJson n = writeFile (n ++ ".json") . toJson
