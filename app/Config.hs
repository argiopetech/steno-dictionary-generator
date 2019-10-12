module Config where

import Keys
import Stroke

import qualified Keys.Left as L
import qualified Keys.Right as R


hundredsModifier = addRight R.Z
reverseModifier  = addVowel U . addVowel E
fingerspellingModifier = addModifier Star
capsModifier = addRight R.P
