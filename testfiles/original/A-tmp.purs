module A where

data Zero   

data ZeroWith   a

data ZeroKind (f :: Type -> Type)

data Crazy = Crazy { 
  test :: String,    idk :: Array Int, 
    hello::{world::String} 
}

type Normal = { 
  test :: String,    idk :: Array Int, 
    hello::{world::String} 
}

data One =
  -- between
  One 

