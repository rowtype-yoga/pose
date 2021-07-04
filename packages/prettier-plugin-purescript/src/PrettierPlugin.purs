module PrettierPlugin (languages, parsers, printers
, options
, defaultOptions
  ) where

import Prelude

import Data.Function.Uncurried (mkFn3)
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Aff.Compat (EffectFn3, mkEffectFn3)
import Format (format)
import PluginOptions as Options
import Prettier (FastPath, PrettierOption, PrettierPluginOptions, nodeFromPath)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types as CST
import Record as Record
import Settings (defaultSettings)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

options :: PrettierPluginOptions (PureScriptOptions PrettierOption ())
options = Options.options

defaultOptions = Options.defaultOptions

type DefaultOptions r = (tabWidth :: Int | r)

type PureScriptOptions :: forall k. k -> Row k -> Row k
type PureScriptOptions a r = (symbolStyle :: a | r)

-- type Options = 
--   Record (PureScriptOptions String )
-- | https://github.com/github/linguist/blob/master/lib/linguist/languages.yml#L4505-L4514
languages =  [
  {
    name: "PureScript",
    parsers: ["purescript"],
    since: "1.0.0", -- ??
    extensions: [".purs"],
    tmScope: "source.purescript",
    aceMode: "haskell",
    linguistLanguageId: 302,
    vscodeLanguageIds: ["purescript"]
  }
]

parsers = {
  purescript: {
    parse: mkFn3 parse,
    astFormat: "purescript-cst",
    locStart: _.start,
    locEnd: _.end
  }
}

parse :: forall t34 t37.  String -> t34 -> t37 -> PSPrettierNode
parse text parsers options = do
  let parsed = parseModule text
  { ast_type: "purescript-cst"
  , body: parsed
  , end: String.length text
  , source: text
  , start: 0
  } 
  where
  settings = optionsToSettings options
  optionsToSettings _ = defaultSettings

type PrettierNode a = 
  { ast_type :: String 
  , body :: a
  , end :: Int
  , start :: Int
  , source :: String
  }

type PSPrettierNode = PrettierNode (RecoveredParserResult CST.Module)

printPureScript :: forall x. FastPath -> x -> {} -> Effect String
printPureScript (path :: FastPath) _ options = do
  node :: PSPrettierNode <- nodeFromPath path
  pure case node.ast_type of
    "purescript-cst" -> case node.body of
      ParseSucceeded m -> format settings m
      _ -> node.source
    _ -> 
      node.source
  where
  settings = optionsToSettings options
  optionsToSettings _ = defaultSettings

printers :: forall t38.
  { "purescript-cst" :: { print :: EffectFn3 FastPath t38 (Record ()) String
                        }
  }
printers = {
  "purescript-cst": {
    print: mkEffectFn3 printPureScript
  }
}