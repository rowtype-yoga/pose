module A where

import PureScript.CST.Types (Expr(..))
import Prelude

data Zero   

data ZeroWith   a

data ZeroKind (f :: Type -> Type)

data Crazy = Crazy { 
  test :: String,    idk :: Array Int, 
    hello::{world::String} 
}

data Cracy = Cracy { baby :: Int }

type Normal = { 
  test :: String,    idk :: Array Int, 
    hello::{world::String} 
}

newtype New = New String 

newtype Eddy = Eddy
  { hey 
    :: Int }

data One =
  -- between
  One 

class 
  ( Eq a
  , Ord a) <= Test a  where 
  please :: String -> a

class 
  ( Eq a
  , Ord b) <= Test2 a b 
    | a -> b  where 
  test1 :: 
    String -> 
    { a :: a, b :: b }
  test2 :: String -> { a :: a, b :: b }

class Test a <= Why a where another :: a -> Int 

instance eqOne :: Eq One where
  eq _ _ = true