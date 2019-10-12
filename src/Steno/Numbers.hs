module Steno.Numbers where

import Dictionary
import Stroke
import Keys
import qualified Keys.Left as L
import qualified Keys.Right as R

import Data.List (subsequences)


n1 = Entry "1" $ [Stroke [L.S] [] [] [Hash]]
n2 = Entry "2" $ [Stroke [L.T] [] [] [Hash]]
n3 = Entry "3" $ [Stroke [L.P] [] [] [Hash]]
n4 = Entry "4" $ [Stroke [L.H] [] [] [Hash]]
n5 = Entry "5" $ [Stroke [] [A] [] [Hash]]
n0 = Entry "0" $ [Stroke [] [O] [] [Hash]]
n6 = Entry "6" $ [Stroke [] [] [R.Fvs] [Hash]]
n7 = Entry "7" $ [Stroke [] [] [R.P] [Hash]]
n8 = Entry "8" $ [Stroke [] [] [R.L] [Hash]]
n9 = Entry "9" $ [Stroke [] [] [R.T] [Hash]]

numbers = [n1, n2, n3, n4, n5, n0, n6, n7, n8, n9]


hundreds hundredsModifier = map go numbers
  where go (Entry n s) =
          Entry (n ++ "00") $ map hundredsModifier s


reverses reverseModifier =
  let pairs = filter ((== 2) . length) $ subsequences numbers
  in map (foldl1 appendEntries) pairs
  where appendEntries (Entry n1 ss1) (Entry n2 ss2) =
          Entry (n2 ++ n1)
                (map reverseModifier $ zipWith (<>) ss1 ss2)
