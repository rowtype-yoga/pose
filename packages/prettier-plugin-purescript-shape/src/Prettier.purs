module Prettier (languages, parsers, printers
  , FastPath
  ) where

import Prelude

import Data.Either (hush)
import Data.Function.Uncurried (mkFn3)
import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Aff.Compat (EffectFn3, mkEffectFn1, mkEffectFn3)
import Format (format)
import Main (parseAndFormat')
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types as CST
import Settings (defaultSettings)

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

foreign import nodeFromPath :: forall a. FastPath -> Effect (PrettierNode a)

foreign import data FastPath :: Type

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