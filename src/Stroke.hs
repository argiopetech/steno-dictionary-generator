{-# LANGUAGE NoMonomorphismRestriction #-}
module Stroke where

import Keys

import qualified Keys.Left as L
import qualified Keys.Right as R

import Data.List (sort, nub, partition)
import Data.Set (Set)

import qualified Data.Set as S


data Stroke = Stroke (Set LeftKey) (Set Vowel) (Set RightKey) (Set Modifier)
  deriving (Eq, Ord)

lefts :: [LeftKey] -> Stroke
lefts ks = Stroke (S.fromList ks) mempty mempty mempty

rights :: [RightKey] -> Stroke
rights ks = Stroke mempty mempty (S.fromList ks) mempty

vowels :: [Vowel] -> Stroke
vowels ks = Stroke mempty (S.fromList ks) mempty mempty

modifiers :: [Modifier] -> Stroke
modifiers ks = Stroke mempty mempty mempty (S.fromList ks)

addLeft :: LeftKey -> Stroke -> Stroke
addLeft k (Stroke ls vs rs ms) = Stroke (k `S.insert` ls) vs rs ms

addVowel :: Vowel -> Stroke -> Stroke
addVowel k (Stroke ls vs rs ms) = Stroke ls (k `S.insert` vs) rs ms

addRight :: RightKey -> Stroke -> Stroke
addRight k (Stroke ls vs rs ms) = Stroke ls vs (k `S.insert` rs) ms

addModifier :: Modifier -> Stroke -> Stroke
addModifier k (Stroke ls vs rs ms) = Stroke ls vs rs (k `S.insert` ms)

class Strokable k where
  stk  :: k -> Stroke
  stks :: [k] -> Stroke
  stks = mconcat . map stk

instance Strokable Stroke where
  stk = id
  stks = mconcat

instance Strokable LeftKey where
  stk k = lefts [k]

instance Strokable Vowel where
  stk k = vowels [k]

instance Strokable RightKey where
  stk k = rights [k]

instance Strokable Modifier where
  stk k = modifiers [k]


class Replace k where
  rep :: [k] -> Stroke -> Stroke

instance Replace LeftKey where
  rep ks (Stroke _ vs rs ms) = Stroke (S.fromList ks) vs rs ms

instance Replace Vowel where
  rep ks (Stroke ls _ rs ms) = Stroke ls (S.fromList ks) rs ms
  
instance Replace RightKey where
  rep ks (Stroke ls vs _ ms) = Stroke ls vs (S.fromList ks) ms

instance Replace Modifier where
  rep ks (Stroke ls vs rs _) = Stroke ls vs rs (S.fromList ks)


instance Semigroup Stroke where
  (<>) (Stroke l1 v1 r1 m1) (Stroke l2 v2 r2 m2) =
    Stroke (l1 <> l2) (v1 <> v2) (r1 <> r2) (m1 <> m2)


instance Monoid Stroke where
  mempty = Stroke mempty mempty mempty mempty
  

instance Show Stroke where
  showsPrec _ = showString . showStroke

showStroke (Stroke ls vs rs ms) =
  let (lvs, hvs) = S.partition (< E) vs
      hasHash = S.member Hash ms
      hasStar = S.member Star ms
      anyNumeric = S.member True $ S.map hasNum ls
                                 <> S.map hasNum vs
                                 <> S.map hasNum rs
      hash = if hasHash && not anyNumeric then show Hash else mempty
      star = if hasStar then show Star else mempty
      hyphen = if not hasStar && S.null vs && not (null rs)
                 then "-" else mempty
      showAll ss =
        let css = map (if hasHash then toNums else show) $ S.toAscList ss
        in concat css
  in hash ++ showAll ls
          ++ showAll lvs
          ++ star ++ hyphen
          ++ showAll hvs
          ++ showAll rs


class HasNumericRepresentation n where
  toNums :: n -> String
  hasNum :: n -> Bool

instance HasNumericRepresentation LeftKey where
  toNums L.S = "1"
  toNums L.T = "2"
  toNums L.P = "3"
  toNums L.H = "4"
  toNums k = show k

  hasNum L.S = True
  hasNum L.T = True
  hasNum L.P = True
  hasNum L.H = True
  hasNum _   = False

instance HasNumericRepresentation Vowel where
  toNums A = "5"
  toNums O = "0"
  toNums k = show k

  hasNum A = True
  hasNum O = True
  hasNum _ = False

instance HasNumericRepresentation RightKey where
  toNums R.Fvs = "6"
  toNums R.P   = "7"
  toNums R.L   = "8"
  toNums R.T   = "9"
  toNums k = show k

  hasNum R.Fvs = True
  hasNum R.P   = True
  hasNum R.L   = True
  hasNum R.T   = True
  hasNum _ = False
