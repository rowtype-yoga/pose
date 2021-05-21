module Pose.PrettierPlugin
  ( languages
  , parsers
  , printers
  , options
  , defaultOptions
  ) where

import Prelude
import Data.Function.Uncurried (mkFn3)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Aff.Compat (mkEffectFn3)
import Pose.Format (format)
import Pose.PluginOptions (AllOptions, symbolStyleASCII, symbolStyleUnicode)
import Pose.PluginOptions as Options
import Prettier (FastPath, PrettierOption, PrettierPluginOptions, nodeFromPath)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types as CST

options ∷
  PrettierPluginOptions
    ( pureScriptSymbolStyle ∷ PrettierOption
    )
options = Options.options

defaultOptions ∷ { tabWidth ∷ Int }
defaultOptions = Options.defaultOptions

type DefaultOptions r
  = ( tabWidth ∷ Int | r )

languages ∷
  Array
    { aceMode ∷ String
    , extensions ∷ Array String
    , linguistLanguageId ∷ Int
    , name ∷ String
    , parsers ∷ Array String
    , since ∷ String
    , tmScope ∷ String
    , vscodeLanguageIds ∷ Array String
    }
languages =
  [ { name: "PureScript"
    , parsers: [ "purescript" ]
    , since: "1.0.0"
    , extensions: [ ".purs" ]
    , tmScope: "source.purescript"
    , aceMode: "haskell"
    , linguistLanguageId: 302
    , vscodeLanguageIds: [ "purescript" ]
    }
  ]

parsers ∷ _
parsers =
  { purescript:
      { parse: mkFn3 parse
      , astFormat: "purescript-cst"
      , locStart: _.start
      , locEnd: _.end
      }
  }

parse ∷ ∀ t34 t37. String → t34 → t37 → PSPrettierNode
parse text _ _ = do
  { ast_type: "purescript-cst"
  , body: parseModule text
  , end: String.length text
  , source: text
  , start: 0
  }

type PrettierNode a
  = { ast_type ∷ String
    , body ∷ a
    , end ∷ Int
    , start ∷ Int
    , source ∷ String
    }

type PSPrettierNode
  = PrettierNode (RecoveredParserResult CST.Module)

printPureScript ∷ FastPath → AllOptions → _ → Effect String
printPureScript path { pureScriptSymbolStyle, tabWidth } _ = do
  node ∷ PSPrettierNode ← nodeFromPath path
  pure case node.ast_type of
    "purescript-cst" → case node.body of
      ParseSucceeded m → format settings m
      _ → node.source
    _ →
      node.source
  where
  settings = { tabSize: power " " tabWidth, sourceStyle }

  sourceStyle = case pureScriptSymbolStyle of
    s | s == symbolStyleUnicode → Just CST.Unicode
    s | s == symbolStyleASCII → Just CST.ASCII
    _ → Nothing

printers ∷ _
printers =
  { "purescript-cst":
      { print: mkEffectFn3 printPureScript
      }
  }
