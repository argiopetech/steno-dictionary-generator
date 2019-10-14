module Config where

import Keys
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R


hundredsModifier = addRight R.Z
reverseModifier  = addVowel U . addVowel E
fingerspellingModifier = addModifier Star
capsModifier = addRight R.Fvs

shiftModifier = capsModifier . fingerspellingModifier
controlModifier = addRight R.R . fingerspellingModifier
controlRModifier = addRight R.Z . controlModifier
altModifier = addRight R.L . fingerspellingModifier
superModifier = addRight R.S . fingerspellingModifier

shift k = "shift(" ++ k ++ ")"
control k = "Control_L(" ++ k ++ ")"
controlR k = "Control_R(" ++ k ++ ")"
alt k = "alt(" ++ k ++ ")"
super k = "super(" ++ k ++ ")"

