-- These are some comments.
{- Here's a block comment.
It has multiple lines.
-}
module ModuleComments where

-- This comment is on an import
import Prelude

{- A comment on a class -}
class Foo

-- A Block comment on a data.
data X
  = X -- A line comment on a constructor
  | X2 {- A block comment on a construction -}         {-Another block comment on the same constructor-}
  | X3      -- A lot of spaces

{- | A Line comment on a newtype -}
newtype Y
  = Y X

-- A Line comment on a fixity
infix 0 y as !

{- A Block comment on a foreign -}
foreign import kind Bar

-- A Line comment on an instance
instance foo ::
  Foo

{- A Block comment on a synonym -}
type Z
  = Y

-- | Another line comment.
x = X

{- A block comment on a type -}
y :: Y -> Y -> Y
y _ _ = Y

-- A comment should not make the next line indent so it's invalid
type MyRow :: Symbol -> Row Type
type MyRow s = s

-- A comment should not make the next line indent so it's invalid
type MyRow :: 
  Symbol -> Row Type
type MyRow s = s

commentInRecord =
  { -- don't destroy the code below
  x: 12
  }

commentInRecord2 =
  { y: 12
  , -- don't destroy the code below either
  x: 12
  }

commentInRecord3 =
  { y: 12
  , 
  x: 12 -- don't destroy the code below either
  , z: 8
  }

commentInRecord4 =
  { y: 12
  , 
  x: 12 -- don't destroy the closing curly
  }

commentInRecord5 =
  { y: 12
  , 
  x: 12
  -- don't destroy the closing curly
  }

commentInArray =
  [ -- don't destroy the code below
  12
  ]

commentInArray2 =
  [ 12
  , -- don't destroy the code below either
  12
  ]

commentInArray3 =
  [  12
  , 
  12 -- don't destroy the code below either
  ,  8
  ]

-- A trailing comment on the module
-- Another trailing module comment


{- A trailing block comment on the module -}
{- One more
  trailing block comment
        across multiple lines
-}
-- | And another trailing line comment