module Test.Spec.Assertions.Extra
  ( isRight
  ) where

import Prelude
import Data.Either (Either, isLeft)
import Effect.Aff (Aff)
import Test.Spec.Assertions (fail)

isRight ∷ ∀ a b. Show a ⇒ Show b ⇒ Either a b → Aff Unit
isRight x =
  when (isLeft x)
    $ fail
    $ (show x)
    <> " is not Right value"
