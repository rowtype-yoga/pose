module Format (format) where

import Prelude

import Control.Lazy (defer)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Debug (spy)
import PureScript.CST.Print as Print
import PureScript.CST.Print as Print
import PureScript.CST.Range (tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types as CST

-- StringBuilder material
type Indentation = Int 

format :: CST.Module Void -> String
format = formatModule 0

formatModule :: Indentation -> CST.Module Void -> String
formatModule indentation (CST.Module { header, body }) =
  printModuleHeader header 
    <> formatModuleBody indentation body

-- [FIXME] This prints as-is, does not format anything
printModuleHeader :: CST.ModuleHeader Void -> String 
printModuleHeader (CST.ModuleHeader header) =
  foldMap Print.printSourceToken (TokenList.toArray $
    TokenList.cons header.keyword
      $ tokensOf header.name
        <> defer (\_ -> foldMap tokensOf header.exports)
        <> TokenList.singleton header.where
        <> defer (\_ -> foldMap tokensOf header.imports))
     
-- Language.PureScript.CST.Types.DeclData span dataHead' dataCtors' -> do
--     let indent' = indent'' <> indentation
--     debug log "DeclData" declaration' span
--     dataHead log indentation indent'' dataHead'
--       <> foldMap
--         ( \(equals, dataCtors) ->
--             pure (newline <> indent')
--               <> sourceToken log indent' blank equals
--               <> separated
--                 log
--                 Span.MultipleLines
--                 indent'
--                 blank
--                 (dataCtor log indentation indent')
--                 dataCtors
--         )
--         dataCtors'
--       <> pure newline

-- dataHead ::
--   Log.Handle ->
--   Indentation ->
--   Indent ->
--   Language.PureScript.CST.Types.DataHead Span.Span ->
--   IO Utf8Builder
-- dataHead log indentation indent dataHead' = case dataHead' of
--   Language.PureScript.CST.Types.DataHead data' name' typeVarBindings -> do
--     debug log "DataHead" dataHead' Span.SingleLine
--     sourceToken log indent blank data'
--       <> name log indent space name'
--       <> foldMap
--         ( \typeVarBinding' ->
--             pure space
--               <> typeVarBinding log indentation indent typeVarBinding'
--         )
--         typeVarBindings

formatDataHead :: Indentation -> String -> CST.DataHead Void -> String
formatDataHead indentation indent { keyword, name: CST.Name { token: nameToken }, vars } = do
  Print.printToken keyword.value <> " " 
    <> Print.printSourceToken nameToken
    <> ?help vars


formatModuleBody :: Indentation -> CST.ModuleBody Void -> String
formatModuleBody indentation (CST.ModuleBody {decls, trailingComments}) = 
  foldMap (formatDeclaration indentation) decls
    <> printTrailingComments trailingComments

printTrailingComments :: Array (CST.Comment CST.LineFeed) -> String
printTrailingComments = foldMap (Print.printComment Print.printLineFeed)

-- head: {
--   keyword: {
--     range: { start: { line: 2, column: 0 }, end: { line: 2, column: 4 } },
--     leadingComments: [ Line { value0: LF {}, value1: 2 } ],
--     trailingComments: [ Space { value0: 1 } ],
--     value: TokLowerName { value0: Nothing {}, value1: 'data' }
--   },
--   name: {
--     token: {
--       range: { start: { line: 2, column: 5 }, end: { line: 2, column: 9 } },
--       leadingComments: [],
--       trailingComments: [],
--       value: TokUpperName { value0: Nothing {}, value1: 'Zero' }
--     },
--     name: 'Zero'
--   },
--   vars: []
-- }
-- resr: Nothing {}
formatDeclaration :: Indentation -> CST.Declaration Void -> String
formatDeclaration indentation = case _ of
  -- CST.DeclData (CST.DataHead Void) (Maybe (Tuple CST.SourceToken (CST.Separated (CST.DataCtor Void)))) -> 
  CST.DeclData (head@{ keyword, name, vars }) rest -> do
    let
       _ = spy "head" head 
       _ = spy "resr" rest 
    (formatDataHead indentation blank head)
  _ -> "???"
    
newline = "\n"

blank = ""

-- | Default: two spaces
indent :: Indentation -> String 
indent = power "  " 