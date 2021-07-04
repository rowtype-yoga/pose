module PluginOptions where

import Prettier (PrettierOption, PrettierPluginOptions, choiceOption, globalCategory, mkPrettierPluginOptions)
import Unsafe.Coerce (unsafeCoerce)

options :: PrettierPluginOptions (PureScriptOptions PrettierOption ())
options = mkPrettierPluginOptions {
symbolStyle: choiceOption {
    choices: [
      { value: symbolStyleKeep, description: "Leaves symbols as they are in the code" },
      { value: symbolStyleUnicode, description: "Prints all symbols as unicode"  },
      { value: symbolStyleASCII, description: "Prints all symbols as ASCII" }
    ],
    category: globalCategory,
    default: symbolStyleKeep,
    description: "Whether to replace arrows etc. with their ASCII/Unicode counterparts"
  }
}

foreign import data SymbolStyleOption :: Type
symbolStyleKeep :: SymbolStyleOption
symbolStyleKeep = unsafeCoerce "keep"
symbolStyleUnicode :: SymbolStyleOption
symbolStyleUnicode = unsafeCoerce "unicode"
symbolStyleASCII :: SymbolStyleOption
symbolStyleASCII = unsafeCoerce "ascii"

defaultOptions :: Record (DefaultOptions ())
defaultOptions = {
  tabWidth: 2
}

type DefaultOptions r = (tabWidth :: Int | r)

type PureScriptOptions :: forall k. k -> Row k -> Row k
type PureScriptOptions a r = (symbolStyle :: a | r)
