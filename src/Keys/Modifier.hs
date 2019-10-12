module Keys.Modifier where

data Modifier = Star | Hash
  deriving (Eq)

instance Show Modifier where
  showsPrec _ Star = showString "*"
  showsPrec _ Hash = showString "#"
