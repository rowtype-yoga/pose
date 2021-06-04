module Format (format) where

import Prelude

import Control.Lazy (defer)
import Data.Foldable (foldMap)
import Debug (spy)
import PureScript.CST.Print as Print
import PureScript.CST.Range (rangeOf, tokensOf, class RangeOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types as CST
import Data.Tuple.Nested ((/\), type (/\))

data Lines = SingleLine | MultiLine 

singleOrMultiline :: forall a. RangeOf a => a -> Lines
singleOrMultiline value = lines
  where
    { start, end } = rangeOf value
    lines = if start.line == end.line then SingleLine else MultiLine

format :: CST.Module Void -> String
format = formatModule "" -- 0

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

formatModuleBody :: Indentation -> CST.ModuleBody Void -> String
formatModuleBody indentation (CST.ModuleBody {decls, trailingComments}) =
  formatDeclarations indentation decls
  -- foldMap (formatDeclaration indentation) decls
    <> printTrailingComments trailingComments

formatDeclarations
  :: Indentation
  -> Array (CST.Declaration Void)
  -> String
formatDeclarations indentation = 
  foldMap (pure newline <> formatDeclaration indentation blank)

formatDeclaration :: Indentation -> Indent -> CST.Declaration Void -> String
formatDeclaration indentation indent = case _ of
  CST.DeclData head rest -> do
    let
      indented = indent <> indentation
      _ = spy "head" head 
      _ = spy "rest" rest 
    formatDataHead indentation indented head
      <> newline
  _ -> "???"


-- Language.PureScript.CST.Types.DeclData span dataHead' dataCtors' -> do
--     let indent' = indent'' <> indentation
--     debug log "DeclData" declaration' span
--     dataHead log indentation indent'' dataHead'
--       <> foldMap
--         ( \(equals, dataCtors) ->
--             pure (newline <> indent')
--               <> formatSourceToken log indent' blank equals
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

-- | Formats the `data Maybe a` part of `data Maybe a = Just a | Nothing`
formatDataHead :: Indentation -> Indent -> CST.DataHead Void -> String
formatDataHead indentation indent { keyword, name, vars } = 
  formatSourceToken indent blank keyword
    <> formatName indent space name
    <> formatTypeVarBindings indentation indent vars

formatTypeVarBindings 
  :: Indentation 
  -> Indent 
  -> Array (CST.TypeVarBinding Void) 
  -> String
formatTypeVarBindings indentation indent = 
  foldMap (pure space <> formatTypeVarBinding indentation indent)
  
formatTypeVarBinding :: Indentation -> Indent -> CST.TypeVarBinding Void -> String
formatTypeVarBinding indentation indent x = case spy "formatTypeVarBinding" x of
  CST.TypeVarName name -> 
    formatName indent blank name
  CST.TypeVarKinded wrapped -> 
    formatWrapped indent (formatLabeledNameKind indentation indent) wrapped

formatLabeledNameKind
  :: forall n t
   . RangeOf (CST.Name n) 
  => RangeOf t 
  => Indentation
  -> Indent
  -> CST.Labeled (CST.Name n) (CST.Type t) 
  -> String
formatLabeledNameKind indentation indent = 
  formatLabeled 
    indentation
    indent
    (formatName indent blank)
    (formatType indentation)

-- data Type e
--   = TypeVar (Name Ident)
--   | TypeConstructor (QualifiedName Proper)
--   | TypeWildcard SourceToken
--   | TypeHole (Name Ident)
--   | TypeString SourceToken String
--   | TypeRow (Wrapped (Row e))
--   | TypeRecord (Wrapped (Row e))
--   | TypeForall SourceToken (NonEmptyArray (TypeVarBinding e)) SourceToken (Type e)
--   | TypeKinded (Type e) SourceToken (Type e)
--   | TypeApp (Type e) (NonEmptyArray (Type e))
--   | TypeOp (Type e) (NonEmptyArray (Tuple (QualifiedName Operator) (Type e)))
--   | TypeOpName (QualifiedName Operator)
--   | TypeArrow (Type e) SourceToken (Type e)
--   | TypeArrowName SourceToken
--   | TypeConstrained (Type e) SourceToken (Type e)
--   | TypeParens (Wrapped (Type e))
--   | TypeUnaryRow SourceToken (Type e)
--   | TypeError e
formatType
  :: forall a
   . RangeOf a
  => Indentation
  -> Indent
  -> CST.Type a
  -> String
formatType indentation indent t = case t of
  CST.TypeVar name -> 
    formatName indent blank name
  CST.TypeArrow t1 arrow t2 -> do
    let 
      prefix = case lines of
        MultiLine -> newline <> indent
        SingleLine -> space
    formatType indentation indent t1
      <> space
      <> formatSourceToken indent blank arrow
      <> prefix
      <> formatType indentation indent t2
  _ -> "formatType:???"
  where
    lines = singleOrMultiline t

formatLabeled
  :: forall a b
   . RangeOf a 
  => RangeOf b 
  => Indentation
  -> Indent
  -> (a -> String)
  -> (Indent -> b -> String)
  -> CST.Labeled a b
  -> String
formatLabeled indentation indent' formatLabel formatValue labeled =
  formatLabel label
    <> formatSourceToken indent space separator
    <> prefix
    <> formatValue indent value
  where 
  CST.Labeled { label, separator, value } = labeled
  (indent /\ prefix) = case singleOrMultiline labeled of
    MultiLine -> (indent' <> indentation) /\ (newline <> indent') -- indent' <> indentation
    SingleLine -> indent' /\ space
      
-- Ident? Proper?
formatName :: forall a. Indent -> Prefix -> CST.Name a -> String
formatName indent prefix (CST.Name { token }) =
  formatSourceToken indent prefix token

formatSourceToken
  :: Indent
  -> Prefix
  -> CST.SourceToken
  -> String
formatSourceToken indent prefix { leadingComments, trailingComments, value } =
  formatLeadingComments indent prefix leadingComments
    <> prefix <> Print.printToken value
    <> formatTrailingComments prefix trailingComments

formatLeadingComments 
  :: Indent 
  -> Prefix
  -> Array (CST.Comment CST.LineFeed)
  -> String
formatLeadingComments indent prefix = foldMap (formatLeadingComment indent prefix)

formatLeadingComment
  :: Indent
  -> Prefix
  -> CST.Comment CST.LineFeed
  -> String
formatLeadingComment indent prefix = case _ of
  CST.Comment comment -> prefix <> comment <> newline <> indent
  CST.Line _ _ -> blank
  CST.Space _ -> blank

formatTrailingComments :: Prefix -> Array (CST.Comment Void) -> String
formatTrailingComments prefix = foldMap (formatTrailingComment prefix)
    
formatTrailingComment :: Prefix -> CST.Comment Void -> String
formatTrailingComment prefix = case _ of
  CST.Comment comment -> prefix <> space <> comment
  CST.Line _ _ -> blank
  CST.Space _ -> blank

formatWrapped
  :: forall a
   . Indent
  -> (a -> String)
  -> CST.Wrapped a
  -> String
formatWrapped indent formatValue wrapped@(CST.Wrapped { open, value, close }) = 
  formatSourceToken indent blank open
    <> before
    <> formatValue value
    <> after
    <> formatSourceToken indent blank close
  where
  { before, after } = case singleOrMultiline wrapped of
    MultiLine -> { before: space, after: newline <> indent }
    SingleLine -> { before: blank, after: blank }
  
type Indentation = String -- [TODO] Use Numbers
type Indent = String
type Prefix = String

printTrailingComments :: Array (CST.Comment CST.LineFeed) -> String
printTrailingComments = foldMap (Print.printComment Print.printLineFeed)

formatLeadingCommentsOld :: Array (CST.Comment CST.LineFeed)-> String
formatLeadingCommentsOld = foldMap (Print.printComment Print.printLineFeed <<< squashNewlines)
  where
    -- | There shouldn't be more then one empty line!
    squashNewlines (CST.Line l n) = (CST.Line l (min n 2))
    squashNewlines others = others

-- | Squashes spaces to one space
formatInnerTrailingComments :: Array (CST.Comment Void) -> String
formatInnerTrailingComments = formatTrailingComments space

-- | Squashes trailing spaces to blank
formatEndTrailingComments :: Array (CST.Comment Void) -> String
formatEndTrailingComments = formatTrailingComments blank

-- formatTrailingComments :: String -> Array (CST.Comment Void) -> String
-- formatTrailingComments defaultSeparator comments = 
--   if Array.any isComment comments
--   then interComments
--   else defaultSeparator
--   where 
  
--   isComment :: forall l. CST.Comment l -> Boolean
--   isComment = case _ of
--     CST.Comment _ -> true
--     _ -> false

--   interComments = Array.fold do
--     comment <- Array.filter isComment comments
--     [ space, Print.printComment absurd comment ] 

newline :: String
newline = "\n"

blank :: String
blank = ""

space :: String 
space = " "

-- | [TODO] Use numbers
-- | Default: two spaces
-- indent :: Indentation -> String 
-- indent = power "  " 
