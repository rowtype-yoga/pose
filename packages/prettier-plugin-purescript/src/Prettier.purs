module Prettier where

import Prelude

import Effect (Effect)
import Record as Record
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)

foreign import data PrettierPluginOptions :: Row Type -> Type

mkPrettierPluginOptions :: forall r. Homogeneous r PrettierOption => { |r } -> PrettierPluginOptions r
mkPrettierPluginOptions = unsafeCoerce

choiceOption :: forall a.
  { choices :: Array { value :: a, description :: String }
  , category :: PrettierOptionCategory
  , default :: a
  , description :: String
  } -> PrettierOption
choiceOption = unsafeCoerce <<< Record.insert (Proxy :: Proxy "type") "choice" 

foreign import data PrettierOption :: Type

foreign import data PrettierOptionCategory :: Type

globalCategory :: PrettierOptionCategory
globalCategory = unsafeCoerce "Global"

defaultOptions :: Record (DefaultOptions ())
defaultOptions = {
  tabWidth: 2
}

type DefaultOptions r = (tabWidth :: Int | r)

type PureScriptOptions :: forall k. k -> Row k -> Row k
type PureScriptOptions a r = (symbolStyle :: a | r)

foreign import nodeFromPath :: forall a. FastPath -> Effect (PrettierNode a)

foreign import data FastPath :: Type

type PrettierNode a = 
  { ast_type :: String 
  , body :: a
  , end :: Int
  , start :: Int
  , source :: String
  }