module A where

data Zero

data One = One

data Two = 
  Two One One

data Three a
  = ThreeA a 
  | ThreeB