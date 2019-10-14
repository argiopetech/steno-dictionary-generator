module Keys.Modifier where

data Modifier = Hash | Star
  deriving (Eq, Ord)

instance Show Modifier where
  showsPrec _ Hash = showString "#"
  showsPrec _ Star = showString "*"
