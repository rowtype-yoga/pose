module Operators where

foo = filter ((&&) <$> testK relation w <*> evaluate' x1) worlds

bar = ((&&) <$> (||) <*> (&&))

multiline = 
 12 +
          3

withRecord =
  blurb <@>
    { a: 1
    , blurbi: 4
    }