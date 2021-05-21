-- These are some comments.
{- Here's a block comment.
It has multiple lines.
-}
module ModuleComments where

-- This comment is on an import
import Prelude

{- A comment on a class -}
class Foo

-- Two comments
-- one class
class
  Foo

-- Two comments
-- on a more complicated class
class Unfoldable1 t where
  unfoldr1 :: forall a b. (b -> Tuple a (Maybe b)) -> b -> t a

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

fun :: Int
fun =
  identity
    -- please don't put
    -- newlines
    -- in between these
    $ 0

commentGluing :: Int
commentGluing =
  identity
    $ -- leave these comments
      -- on separate lines
      0

commentGluingWorse :: Int
commentGluingWorse =
  identity
    $ -- don't turn the next line into a comment
      0


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

commentedOut = do
  -- (a /\ b)
  (a /\ b) ← pure ("1" /\ "2")
  -- x /\ y
  x /\ y ← pure ("1" /\ "2")
  -- c /\ d
  c /\ d ←
    pure
      ("c" /\ "d")
  pure unit

span = go Nothing
  where
  go i =
    -- comment one
    -- comment 2
    case i of
      -- Everything must be commented
      Just x -> 1 -- here too, why not?
      -- Sometimes even in
      -- greater detail
      Nothing  -- A comment would be good
        -> -- here
        -- what about here?
        2 -- and here?

inlineComments :: IAm -- line comment
  -> Boolean {- block comment -}
  -> Boolean ->
  -- ^ comment after arrow
  Int
  -> BeforeAnArrow
inlineComments = 77

inlineCommentsParens -- inline comments
  :: -- this is a double colon
  forall a b m -- for all of them
  . Eq a -- equals?
  => Ord -- ordered?
    a =>
  (e -> Maybe b) -- ^ I really like this
  -> m a -- ^ I really like this, too
  -> { a :: Int } -- ^ And this
  -> Maybe b -- ^ I really like this, too
  -> _ -- ^ I really like this, too
  -> m _ -- Oh no, a hole
  -> Int
inlineCommentsParens = 1

nested =
  insert (Tuple "1" "2")
  where
  insertSep state@(Tuple stk acc) = case tpl of
    -- Comment 1
    Tuple lytPos LytTopDecl | sepP lytPos ->
      1
    -- Comment 2 3
    Tuple what a : b ->
      2
    -- Comment with more lines
    Tuple what a
      : b -> 3

-- A trailing comment on the module
-- Another trailing module comment


{- A trailing block comment on the module -}
{- One more
  trailing block comment
        across multiple lines
-}
-- | And another trailing line comment
