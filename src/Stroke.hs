{-# LANGUAGE NoMonomorphismRestriction #-}
module Stroke where

import Keys
import qualified Keys.Left as L
import qualified Keys.Right as R

import Data.List (sort, nub, partition)


data Stroke = Stroke [LeftKey] [Vowel] [RightKey] [Modifier]

lefts :: [LeftKey] -> Stroke
lefts ks = Stroke ks [] [] []

rights :: [RightKey] -> Stroke
rights ks = Stroke [] [] ks []

vowels :: [Vowel] -> Stroke
vowels ks = Stroke [] ks [] []

modifiers :: [Modifier] -> Stroke
modifiers ks = Stroke [] [] [] ks

addLeft :: LeftKey -> Stroke -> Stroke
addLeft k (Stroke ls vs rs ms) = Stroke (k:ls) vs rs ms

addVowel :: Vowel -> Stroke -> Stroke
addVowel k (Stroke ls vs rs ms) = Stroke ls (k:vs) rs ms

addRight :: RightKey -> Stroke -> Stroke
addRight k (Stroke ls vs rs ms) = Stroke ls vs (k:rs) ms

addModifier :: Modifier -> Stroke -> Stroke
addModifier k (Stroke ls vs rs ms) = Stroke ls vs rs (k:ms)

class Strokable k where
  stk  :: k -> Stroke
  stks :: [k] -> Stroke
  stks = mconcat . map stk

instance Strokable Stroke where
  stk = id
  stks = mconcat

instance Strokable LeftKey where
  stk k = Stroke [k] [] [] []

instance Strokable Vowel where
  stk k = Stroke [] [k] [] []

instance Strokable RightKey where
  stk k = Stroke [] [] [k] []

instance Strokable Modifier where
  stk k = Stroke [] [] [] [k]


class Replace k where
  rep :: [k] -> Stroke -> Stroke

instance Replace LeftKey where
  rep ks (Stroke _ vs rs ms) = Stroke ks vs rs ms

instance Replace Vowel where
  rep ks (Stroke ls _ rs ms) = Stroke ls ks rs ms
  
instance Replace RightKey where
  rep ks (Stroke ls vs _ ms) = Stroke ls vs ks ms

instance Replace Modifier where
  rep ks (Stroke ls vs rs _) = Stroke ls vs rs ks


instance Semigroup Stroke where
  (<>) (Stroke l1 v1 r1 m1) (Stroke l2 v2 r2 m2) =
    Stroke (l1 ++ l2) (v1 ++ v2) (r1 ++ r2) (m1 ++ m2)


instance Monoid Stroke where
  mempty = Stroke [] [] [] []
  

instance Show Stroke where
  showsPrec _ = showString . showStroke

showStroke (Stroke ls vs rs ms) =
  let sls = sortNub ls
      (lvs, hvs) = partition (< E) . sortNub $ vs
      srs = sortNub rs
      hasHash = elem Hash ms
      hasStar = elem Star ms
      anyNumeric = elem True $ map hasNum ls
                            ++ map hasNum vs
                            ++ map hasNum rs
      hash = if hasHash && not anyNumeric then show Hash else mempty
      star = if hasStar then show Star else mempty
      hyphen = if not hasStar && null vs && not (null rs)
                 then "-" else mempty
      showAll ss =
        let css = map (if hasHash then toNums else show) ss
        in concat css
  in hash ++ showAll sls
          ++ showAll lvs
          ++ star ++ hyphen
          ++ showAll hvs
          ++ showAll srs
  where sortNub = sort . nub


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
