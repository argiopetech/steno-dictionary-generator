module Keys.Right where

data RightKey =  Fvs | R | P | B | L | G | T | S | D | Z
  deriving (Enum, Eq, Ord)

instance Show RightKey where
  showsPrec _ Fvs = showString "F"
  showsPrec _ R   = showString "R"
  showsPrec _ P   = showString "P"
  showsPrec _ B   = showString "B"
  showsPrec _ L   = showString "L"
  showsPrec _ G   = showString "G"
  showsPrec _ T   = showString "T"
  showsPrec _ S   = showString "S"
  showsPrec _ D   = showString "D"
  showsPrec _ Z   = showString "Z"
  
