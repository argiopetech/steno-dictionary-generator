module Dictionary where

import Stroke

import Data.List (intercalate)
import Text.Printf


data Entry = Entry String [Stroke]
  deriving (Eq)

entry name strokes = [Entry name strokes]

entryS name strokes suffixes =
  let base = Entry name strokes
      rest = map (\(mName, mStrokes) ->
                    Entry (mName name) (mStrokes strokes)) suffixes
  in base : rest

entries :: [[Entry]] -> [Entry]
entries = concat

prettyPrintStrokes = intercalate "/" . map show

toJson :: [Entry] -> String
toJson = (++ "\n}") . init . init . unlines . ("{":) . map go
  where go (Entry n ss) =
          let strokes = prettyPrintStrokes ss
          in printf "\"%s\": \"%s\"," strokes n
