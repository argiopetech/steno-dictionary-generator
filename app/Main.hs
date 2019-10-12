module Main where

import Config
import Primary

import Dictionary

import Steno.Alphabet
import Steno.Numbers


main :: IO ()
main = do
  writeJson "Numbers" $ hundreds hundredsModifier
                     ++ reverses reverseModifier
  writeJson "Alphabet" $
    fingerspelling fingerspellingModifier capsModifier

  writeJson "Primary" $ primaryDictionary

  where writeJson n = writeFile (n ++ ".json") . toJson
