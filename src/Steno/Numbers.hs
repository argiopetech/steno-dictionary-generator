module Steno.Numbers where

import Dictionary
import Stroke
import Keys
import qualified Keys.Left as L
import qualified Keys.Right as R

import Data.List (subsequences)


n1 = Entry "1" [stk L.S <> stk Hash]
n2 = Entry "2" [stk L.T <> stk Hash]
n3 = Entry "3" [stk L.P <> stk Hash]
n4 = Entry "4" [stk L.H <> stk Hash]
n5 = Entry "5" [stk A <> stk Hash]
n0 = Entry "0" [stk O <> stk Hash]
n6 = Entry "6" [stk R.Fvs <> stk Hash]
n7 = Entry "7" [stk R.P <> stk Hash]
n8 = Entry "8" [stk R.L <> stk Hash]
n9 = Entry "9" [stk R.T <> stk Hash]

numbers = [n1, n2, n3, n4, n5, n0, n6, n7, n8, n9]


doubles doublesModifier = map go numbers
  where go (Entry n s) =
          Entry (n <> n) $ map doublesModifier s
          

hundreds hundredsModifier = map go numbers
  where go (Entry n s) =
          Entry (n ++ "00") $ map hundredsModifier s


reverses reverseModifier =
  let pairs = filter ((== 2) . length) $ subsequences numbers
  in map (foldl1 appendEntries) pairs
  where appendEntries (Entry n1 ss1) (Entry n2 ss2) =
          Entry (n2 ++ n1)
                (map reverseModifier $ zipWith (<>) ss1 ss2)
