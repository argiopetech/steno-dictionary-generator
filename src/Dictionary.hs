module Dictionary where

import Stroke

import Data.List (intercalate)
import Text.Printf


data Entry = Entry String [Stroke]

toJson :: [Entry] -> String
toJson = (++ "\n}") . init . init . unlines . ("{":) . map go
  where go (Entry n ss) =
          let strokes = intercalate "/" . map show $ ss
          in printf "\"%s\": \"%s\"," strokes n
