module Format (format) where

import Prelude

import Control.Lazy (defer)
import Data.Array.NonEmpty as NE
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (un)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import PureScript.CST.Print as Print
import PureScript.CST.Range (rangeOf, tokensOf, class RangeOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types as CST

data Lines = SingleLine | MultiLine 

singleOrMultiline :: forall a. RangeOf a => a -> Lines
singleOrMultiline value = lines
  where
    { start, end } = rangeOf value
    lines = if start.line == end.line then SingleLine else MultiLine

format :: CST.Module Void -> String
format = formatModule "  " -- Indentation is two spaces

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

-- data Declaration e
--   = DeclData (DataHead e) (Maybe (Tuple SourceToken (Separated (DataCtor e))))
--   | DeclType (DataHead e) SourceToken (Type e)
--   | DeclNewtype (DataHead e) SourceToken (Name Proper) (Type e)
--   | DeclClass (ClassHead e) (Maybe (Tuple SourceToken (NonEmptyArray (Labeled (Name Ident) (Type e)))))
--   | DeclInstanceChain (Separated (Instance e))
--   | DeclDerive SourceToken (Maybe SourceToken) (InstanceHead e)
--   | DeclKindSignature SourceToken (Labeled (Name Proper) (Type e))
--   | DeclSignature (Labeled (Name Ident) (Type e))
--   | DeclValue (ValueBindingFields e)
--   | DeclFixity FixityFields
--   | DeclForeign SourceToken SourceToken (Foreign e)
--   | DeclRole SourceToken SourceToken (Name Proper) (NonEmptyArray (Tuple SourceToken Role))
--   | DeclError e
formatDeclaration :: Indentation -> Indent -> CST.Declaration Void -> String
formatDeclaration indentation indent declaration = case declaration of
  CST.DeclData dataHead maybeConstructors -> do
    formatDataHead indentation indented dataHead
      <> foldMap formatDataConstructors maybeConstructors
      <> newline
    where
      formatDataConstructors (equals /\ constructors) = 
        newline 
          <> indented
          <> formatSourceToken indented blank equals
          <> formatSeparated 
              lines
              indented
              blank
              (formatDataConstructor indentation indented)
              constructors      
  CST.DeclType dataHead equals typo ->
    formatDataHead indentation indent dataHead
      <> newline
      <> indented
      <> formatSourceToken indented blank equals
      <> space
      <> formatType indentation (indented <> indentation) typo
      <> newline
  _ -> "???"
  where 
    indented = indent <> indentation
    lines = singleOrMultiline declaration

formatDataConstructor
  :: forall a
   . RangeOf a
  => Indentation
  -> Indent
  -> CST.DataCtor a
  -> String
formatDataConstructor indentation indent constructor@(CST.DataCtor { name, fields }) = 
  space
    <> formatName indent blank name
    <> foldMap formatField fields
  where 
    { indented, prefix } = case singleOrMultiline constructor of
        MultiLine -> { indented: indent <> indentation, prefix: newline <> indent } 
        SingleLine -> { indented: indent, prefix: space }
    
    formatField typo = prefix <> formatType indentation indented typo

-- | Formats the `data Maybe a` part of `data Maybe a = Just a | Nothing`
formatDataHead :: Indentation -> Indent -> CST.DataHead Void -> String
formatDataHead indentation indent { keyword, name, vars } = 
  formatSourceToken indent blank keyword
    <> formatName indent space name
    <> formatTypeVarBindings indentation indent vars

formatTypeVarBindings 
  :: forall a
   . RangeOf a
  => Indentation 
  -> Indent 
  -> Array (CST.TypeVarBinding a) 
  -> String
formatTypeVarBindings indentation indent = 
  foldMap (pure space <> formatTypeVarBinding indentation indent)
  
formatTypeVarBinding
  :: forall a
   . RangeOf a
  => Indentation 
  -> Indent 
  -> CST.TypeVarBinding a
  -> String
formatTypeVarBinding indentation indent x = case spy "formatTypeVarBinding" x of
  CST.TypeVarName name -> 
    formatName indent blank name
  CST.TypeVarKinded wrapped -> 
    formatWrapped indent (formatLabeledName indentation indent) wrapped

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
  CST.TypeConstructor qualifiedName ->
    formatQualifiedName indent blank qualifiedName
  CST.TypeWildcard wildcard ->
    formatSourceToken indent blank wildcard
  CST.TypeHole name -> 
    formatName indent blank name
  CST.TypeString typeString _ -> 
    formatSourceToken indent blank typeString
  CST.TypeRow wrappedRow -> 
    formatWrappedRow indentation indent wrappedRow
  CST.TypeRecord wrappedRow -> 
    formatWrappedRow indentation indent wrappedRow
  CST.TypeForall forAll typeVarBindings dot tailType -> 
    formatSourceToken indent blank forAll
      <> formatTypeVarBindings indentation indent (NE.toArray typeVarBindings)
      <> formatSourceToken indent blank dot
      <> prefix
      <> formatType indentation indent tailType
    where 
      prefix = case lines of
        MultiLine -> newline <> indent
        SingleLine -> space 
  CST.TypeKinded typo colons kind ->
    formatType indentation indent typo 
      <> space
      <> formatSourceToken indent blank colons
      <> prefix
      <> formatType indentation indented kind
    where
      { indented, prefix } = case lines of
        MultiLine -> { indented: indent <> indentation, prefix: newline <> indent } 
        SingleLine -> { indented: indent, prefix: space }
  CST.TypeApp typo types ->
    formatType indentation indent typo
      <> formatTypes types -- [TODO] Verify that this works
    where
      { indented, prefix } = case lines of
        MultiLine -> { indented: indent <> indentation, prefix: newline <> indent } 
        SingleLine -> { indented: indent, prefix: space }

      formatTypes = foldMap (pure prefix <> formatType indentation indented)
  CST.TypeOp typo types ->
    formatType indentation indent typo
      <> foldMap formatOp types -- [TODO] Verify
    where
      { indented, prefix } = case lines of
        MultiLine -> { indented: indent <> indentation, prefix: newline <> indent } 
        SingleLine -> { indented: indent, prefix: space }

      formatOp (op /\ anotherType) = 
        formatQualifiedName indented prefix op
        <> prefix
        <> formatType indentation indented anotherType
  CST.TypeOpName op ->
    formatQualifiedName indent blank op
  CST.TypeArrow t1 arrow t2 -> 
    formatType indentation indent t1
      <> space
      <> formatSourceToken indent blank arrow
      <> prefix
      <> formatType indentation indent t2
    where
      prefix = case lines of
        MultiLine -> newline <> indent
        SingleLine -> space
  CST.TypeArrowName arrowName ->
    formatSourceToken indent blank arrowName
  CST.TypeConstrained constraint arrow typo ->
    formatType indentation indent constraint
      <> space
      <> formatSourceToken indent blank arrow
      <> prefix
      <> formatType indentation indent typo
    where
      prefix = case lines of
        MultiLine -> newline <> indent
        SingleLine -> space
  CST.TypeParens wrapped -> 
    formatParens lines indentation indent (formatType indentation) wrapped
  CST.TypeUnaryRow sourceToken typo ->
    formatSourceToken indent blank sourceToken
      <> prefix
      <> formatType indentation indent typo
    where
      prefix = case lines of
        MultiLine -> newline <> indent
        SingleLine -> space
  CST.TypeError e -> "Oopsie" -- [TODO] absurd e
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
formatLabeled indentation indent formatLabel formatValue labeled =
  formatLabel label
    <> formatSourceToken indented space separator
    <> prefix
    <> formatValue indented value
  where 
  CST.Labeled { label, separator, value } = labeled
  { indented, prefix } = case singleOrMultiline labeled of
    MultiLine -> { indented: indent <> indentation, prefix: newline <> indent }
    SingleLine -> { indented: indent, prefix: space}

formatLabeledName
  :: forall n t
   . RangeOf (CST.Name n) 
  => RangeOf t 
  => Indentation
  -> Indent
  -> CST.Labeled (CST.Name n) (CST.Type t) 
  -> String
formatLabeledName indentation indent = 
  formatLabeled 
    indentation
    indent
    (formatName indent blank)
    (formatType indentation)

formatParens
  :: forall a 
   . Lines
  -> Indentation
  -> Indent
  -> (Indent -> a -> String)
  -> CST.Wrapped a
  -> String
formatParens lines indentation indent formatA wrapped = do
  formatWrapped indent (formatA indented) wrapped
  where
    indented = case lines of
      MultiLine -> indent <> indentation
      SingleLine -> indent

formatRow
  :: forall a 
   . RangeOf a
  => Lines 
  -> Indentation 
  -> Indent 
  -> CST.Row a
  -> String
formatRow lines indentation indent (CST.Row { labels, tail }) = case labels, tail of 
  Nothing, Nothing ->
    blank
  Just ls, Nothing -> 
    before
      <> formatSeparated lines indent space f ls
      <> after
    where 
      { before, indented, after } = 
        case lines of
          MultiLine -> { before: blank, indented: indent <> indentation, after: blank }
          SingleLine -> { before: space, indented: indent, after: space }
      f = formatLabeledName indentation indented
  Nothing, Just (bar /\ tailType) -> do
    before
      <> formatSourceToken indent blank bar
      <> space
      <> formatType indentation indent tailType
      <> after
    where 
      { before, after } = 
        case lines of
          MultiLine -> { before: newline <> indent, after: blank }
          SingleLine -> { before: space, after: space }
  Just ls, Just (bar /\ tailType) -> do
    before
      <> formatSeparated lines indent space f ls
      <> prefix
      <> formatSourceToken indent blank bar
      <> space
      <> formatType indentation indent tailType
      <> after
    where
      { before, indented, after, prefix } = 
        case lines of
          MultiLine -> 
            { before: blank, indented: indent <> indentation, after: blank, prefix: newline <> indent }
          SingleLine -> 
            { before: space, indented: indent, after: space, prefix: space }
      f = formatLabeledName indentation indented
  
formatSeparated
  :: forall a
   . Lines
  -> Indent
  -> Prefix
  -> (a -> String)
  -> CST.Separated a
  -> String
formatSeparated lines indent prefix formatValue (CST.Separated { head, tail } ) = 
  formatValue head <> foldMap go tail
  where
    prefixed = case lines of
      MultiLine -> newline <> indent
      SingleLine -> blank
    go :: CST.SourceToken /\ a -> String
    go (separator /\ value) = 
      prefixed
        <> formatSourceToken indent blank separator
        <> prefix
        <> formatValue value
      
-- Ident? Proper?
formatName :: forall a. Indent -> Prefix -> CST.Name a -> String
formatName indent prefix (CST.Name { token }) =
  formatSourceToken indent prefix token

formatQualifiedName :: forall a. Indent -> Prefix -> CST.QualifiedName a -> String
formatQualifiedName indent prefix (CST.QualifiedName { token }) =
  formatSourceToken indent prefix token
  -- sourceToken log indent prefix qualifiedName'

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
  
formatWrappedRow
  :: forall a 
   . RangeOf a
  => Indentation 
  -> Indent 
  -> CST.Wrapped (CST.Row a)
  -> String
formatWrappedRow indentation indent wrapped@(CST.Wrapped { open, value: row, close }) =
  formatSourceToken indent blank open
      <> before
      <> formatRow lines indentation indent row
      <> after
      <> formatSourceToken indent blank close
  where 
  lines = singleOrMultiline wrapped

  beforeLabel = if isJust $ _.labels $ un CST.Row row then space else blank
    
  { before, after } = case lines of
    MultiLine -> { before: beforeLabel, after: newline <> indent }
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
