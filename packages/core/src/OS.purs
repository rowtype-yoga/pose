module OS
  ( stdinAsString
  ) where

import Effect (Effect)

foreign import stdinAsString ∷ Effect String
