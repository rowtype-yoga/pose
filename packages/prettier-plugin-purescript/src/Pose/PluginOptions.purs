module Pose.PluginOptions where

import Prelude
import Prettier (PrettierOption, PrettierPluginOptions, choiceOption, globalCategory, mkPrettierPluginOptions)
import Type.Row (type (+))

options ∷ PrettierPluginOptions (PureScriptOptions PrettierOption ())
options =
  mkPrettierPluginOptions
    { pureScriptSymbolStyle:
        choiceOption
          { choices:
              [ { value: symbolStyleKeep, description: "Leaves symbols as they are in the code" }
              , { value: symbolStyleUnicode, description: "Prints all symbols as unicode" }
              , { value: symbolStyleASCII, description: "Prints all symbols as ASCII" }
              ]
          , category: globalCategory
          , default: symbolStyleKeep
          , description: "Whether to replace arrows etc. with their ASCII/Unicode counterparts"
          }
    }

newtype SymbolStyleOption
  = SymbolStyleOption String

symbolStyleKeep ∷ SymbolStyleOption
symbolStyleKeep = SymbolStyleOption "keep"

symbolStyleUnicode ∷ SymbolStyleOption
symbolStyleUnicode = SymbolStyleOption "unicode"

symbolStyleASCII ∷ SymbolStyleOption
symbolStyleASCII = SymbolStyleOption "ascii"

derive newtype instance Eq SymbolStyleOption

defaultOptions ∷ Record (DefaultOptions ())
defaultOptions =
  { tabWidth: 2
  }

type DefaultOptions r
  = ( tabWidth ∷ Int | r )

type PureScriptOptions ∷ ∀ k. k → Row k → Row k
type PureScriptOptions a r
  = ( pureScriptSymbolStyle ∷ a | r )

type AllOptions
  = { | DefaultOptions + PureScriptOptions SymbolStyleOption () }
