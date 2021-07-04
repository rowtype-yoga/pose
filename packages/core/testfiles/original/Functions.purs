module Functions where

import Prelude

someFunction a b = a + b

someFunctionWithNewline a b =
         a + b

someMultiLineFunction a b = 
  -- apply a function
    someFunction
         a
         b


aFunctionWithManyParameters 
  a 
  b 
  c 
  d 
  e 
  f = a + b + c

aFunctionWithMixedParameters 
  a b 
  c 
  d e 

  f = a + b + c

aRecordFunction {
  a,
  b
} = a + b 

aDeconstructedFunction blurb@{
  a,
  b
} = a + b 