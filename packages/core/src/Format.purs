module Format (format) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NE
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Semigroup.Foldable (intercalateMap)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CST.Print as Print
import PureScript.CST.Range (class RangeOf, rangeOf)
import PureScript.CST.Types as CST
import Settings (Settings)

data Lines
  = SingleLine| MultipleLines

singleOrMultiline :: forall a. RangeOf a => a -> Lines
singleOrMultiline value = singleOrMultilineFromRange (rangeOf value)

singleOrMultilineFromRange :: CST.SourceRange -> Lines
singleOrMultilineFromRange { start, end } = if start.line == end.line then SingleLine else MultipleLines

singleOrMultilineBetween :: forall a b. RangeOf a => RangeOf b => a -> b -> Lines
singleOrMultilineBetween a b = do
  let
    { start } = rangeOf a
  let
    { end } = rangeOf b
  singleOrMultilineFromRange { start, end }

rangeOfInstanceHead :: CST.InstanceHead Void -> Lines
rangeOfInstanceHead { keyword, className, types } = do
  let
    { end } = case Array.last types of
      Nothing -> rangeOf className
      Just ty -> rangeOf ty
  singleOrMultilineFromRange
    { start: keyword.range.start
    , end
    }

rangeOfPatternGuard :: (CST.PatternGuard Void) -> CST.SourceRange
rangeOfPatternGuard (CST.PatternGuard { binder, expr }) = do
  let
    { start } = maybe (rangeOf expr) (fst >>> rangeOf) binder

    { end } = rangeOf expr
  { start, end }

rangeOfSeparated ::
  forall a.
  (a -> CST.SourceRange) ->
  CST.Separated a ->
  CST.SourceRange
rangeOfSeparated f separated = case separated of
  CST.Separated { head, tail } -> do
    let
      headRange = f head
    let
      tailRange = maybe headRange f (Array.last tail <#> snd)
    { start: headRange.start, end: tailRange.end }

format :: Settings -> CST.Module Void -> String
format = formatModule -- Indentation is two spaces

formatModule :: Settings -> CST.Module Void -> String
formatModule settings (CST.Module { header, body }) =
  formatModuleHeader settings header
    <> formatModuleBody settings body

-- [FIXME] This prints as-is, does not format anything
formatModuleHeader :: Settings -> CST.ModuleHeader Void -> String
formatModuleHeader settings (CST.ModuleHeader header) =
  formatSourceToken settings blank blank header.keyword
    <> formatName settings blank space header.name
    <> formatExports settings header.exports
    <> formatSourceToken settings blank space header.where
    <> newline
    <> formatImports settings header.imports

formatExports ::
  Settings ->
  Maybe (CST.DelimitedNonEmpty (CST.Export Void)) ->
  String
formatExports settings@{ indentation } exports'' = case exports'' of
  Nothing -> blank
  Just exports' -> do
    let
      indent = indentation

      prefix = case lines of
        MultipleLines -> newline <> indent
        SingleLine -> space
    prefix
      <> formatDelimitedNonEmpty
          settings
          lines
          indent
          (formatExport settings indent)
          exports'
    where
    lines = singleOrMultiline exports'

formatExport ::
  Settings ->
  Indent ->
  CST.Export Void ->
  String
formatExport settings@{ indentation } indent' export' = case export' of
  CST.ExportClass class' name' -> do
    formatSourceToken settings indent' blank class'
      <> formatName settings indent' space name'
  CST.ExportKind kind name' -> do
    formatSourceToken settings indent' blank kind
      <> formatName settings indent' space name'
  CST.ExportModule module'' name' -> do
    formatSourceToken settings indent' blank module''
      <> formatName settings indent' space name'
  CST.ExportOp name' -> do
    formatName settings indent' blank name'
  CST.ExportType name' dataMembers' -> do
    let
      indentTrick = indent' <> indentation

      indent /\ prefix = case lines of
        MultipleLines -> indentTrick /\ (newline <> indentTrick)
        SingleLine -> indent' /\ blank
    formatName settings indent' blank name'
      <> prefix
      <> foldMap (formatDataMembers settings indent) dataMembers'
  CST.ExportTypeOp type'' name' -> do
    formatSourceToken settings indent' blank type''
      <> formatName settings indent' space name'
  CST.ExportValue name' -> do
    formatName settings indent' blank name'
  CST.ExportError e -> absurd e
  where
  lines = singleOrMultiline export'

formatImports ::
  Settings ->
  Array (CST.ImportDecl Void) ->
  String
formatImports settings imports' = case imports' of
  [] -> blank
  _ -> do
    let
      indent = blank
    foldMap
      ( \importDecl ->
          newline
            <> formatImportDeclaration settings indent importDecl
      )
      imports'
      <> newline

formatImportDeclaration ::
  Settings ->
  Indent ->
  CST.ImportDecl Void ->
  String
formatImportDeclaration settings@{ indentation } indent'' importDecl' = case importDecl' of
  CST.ImportDecl { keyword: import'', module: name'', names: imports'', qualified: rename } -> do
    let
      span = singleOrMultiline importDecl'
    let
      indent' = indent'' <> indentation
    formatSourceToken settings indent'' blank import''
      <> formatName settings indent'' space name''
      <> foldMap
          ( \(hiding' /\ imports') -> case hiding' of
              Just hiding -> do
                let
                  hidingPrefix = case span of
                    MultipleLines -> newline <> indent'
                    SingleLine -> space

                  importPrefix = case span of
                    MultipleLines -> newline <> indent
                    SingleLine -> space

                  indent = indent' <> indentation
                hidingPrefix
                  <> formatSourceToken settings indent' blank hiding
                  <> importPrefix
                  <> formatDelimitedNonEmpty
                      settings
                      span -- [TODO] Probably a different span is needed here
                      indent
                      (formatImport settings indent')
                      imports'
              Nothing -> do
                let
                  importPrefix = case span of
                    MultipleLines -> newline <> indent'
                    SingleLine -> space
                importPrefix
                  <> formatDelimitedNonEmpty
                      settings
                      span -- [TODO] Probably a different span is needed here
                      indent'
                      (formatImport settings indent')
                      imports'
          )
          imports''
      <> foldMap
          ( \(as /\ name') -> do
              let
                prefix = case span of
                  MultipleLines -> newline <> indent'
                  SingleLine -> space
              prefix
                <> formatSourceToken settings indent' blank as
                <> formatName settings indent' space name'
          )
          rename

formatImport ::
  Settings ->
  Indent ->
  CST.Import Void ->
  String
formatImport settings@{ indentation } indent' import'' = case import'' of
  CST.ImportClass class' name' -> do
    formatSourceToken settings indent' blank class'
      <> formatName settings indent' space name'
  CST.ImportKind kind name' -> do
    formatSourceToken settings indent' blank kind
      <> formatName settings indent' space name'
  CST.ImportOp name' -> do
    formatName settings indent' blank name'
  CST.ImportType name' dataMembers' -> do
    let
      indent = case lines of
        MultipleLines -> indent' <> indentation
        SingleLine -> indent'
    formatName settings indent' blank name'
      <> foldMap (formatDataMembers settings indent) dataMembers'
  CST.ImportTypeOp type'' name' -> do
    formatSourceToken settings indent' blank type''
      <> formatName settings indent' space name'
  CST.ImportValue name' -> do
    formatName settings indent' blank name'
  CST.ImportError e -> absurd e
  where
  lines = singleOrMultiline import''

formatDataMembers ::
  Settings ->
  Indent ->
  CST.DataMembers ->
  String
formatDataMembers settings@{ indentation } indent' dataMembers' = case dataMembers' of
  CST.DataAll sourceToken' -> do
    formatSourceToken settings indent' blank sourceToken'
  CST.DataEnumerated delimited' -> do
    let
      (indent /\ prefix) = case span of
        MultipleLines -> (indent' <> indentation) /\ (newline <> indent')
        SingleLine -> (indent' /\ blank)
    prefix
      <> formatDelimited
          settings
          indent'
          (formatName settings indent blank)
          delimited'
  where
  span = singleOrMultiline dataMembers'

formatDelimited ::
  forall a.
  Settings ->
  Indent ->
  (a -> String) ->
  CST.Delimited a ->
  String
formatDelimited settings indent formatValue delimited = do
  formatWrapped
    settings
    indent
    (foldMap (formatSeparated settings lines indent space formatValue))
    delimited
  where
  lines = singleOrMultiline delimited

formatModuleBody :: Settings -> CST.ModuleBody Void -> String
formatModuleBody settings@{ indentation } (CST.ModuleBody { decls, trailingComments }) =
  formatDeclarations settings decls
    <> formatCommentsTrailingModule trailingComments

formatDeclarations ::
  Settings ->
  Array (CST.Declaration Void) ->
  String
formatDeclarations settings@{ indentation } = foldMap formatOne
  where
  formatOne declaration = newline <> formatDeclaration settings blank declaration

formatDeclaration :: Settings -> Indent -> CST.Declaration Void -> String
formatDeclaration settings@{ indentation } indent declaration = case declaration of
  CST.DeclData dataHead maybeConstructors -> do
    formatDataHead settings indent dataHead
      <> foldMap formatDataConstructors maybeConstructors
      <> newline
    where
    formatDataConstructors (equals /\ constructors) =
      (newline <> indented)
        <> formatSourceToken settings indented blank equals
        <> formatSeparated
            settings
            (singleOrMultiline declaration)
            indented
            blank
            (formatDataConstructor settings indented)
            constructors
  CST.DeclType dataHead equals typo ->
    formatDataHead settings indent dataHead
      <> newline
      <> indented
      <> formatSourceToken settings indented blank equals
      <> space
      <> formatType settings (indented <> indentation) (singleOrMultiline typo) typo
      <> newline
  CST.DeclNewtype dataHead equals name typo -> do
    formatDataHead settings indent dataHead
      <> (newline <> indented)
      <> formatSourceToken settings indented blank equals
      <> space
      <> formatName settings indented blank name
      <> prefix
      <> formatType settings indented lines typo
      <> newline
    where
    prefix = case singleOrMultilineBetween name typo of
      MultipleLines -> newline <> indented
      SingleLine -> space
  CST.DeclClass classHead members -> do
    formatClassHead lines settings indent classHead
      <> foldMap formatMember members
      <> newline
    where
    formatMember (whereClause /\ labeleds) =
      formatSourceToken settings indent space whereClause
        <> intercalateMap (newline <> indented) formatLabeledMember labeleds

    formatLabeledMember labeled = newline <> indented <> formatLabeledNameType settings indented labeled
  CST.DeclInstanceChain instances ->
    formatSeparated
      settings
      (singleOrMultiline instances)
      indent
      space
      (formatInstance settings indent)
      instances
      <> newline
  CST.DeclDerive sourceToken newtype' instanceHead ->
    formatSourceToken settings indent blank sourceToken
      <> foldMap (formatSourceToken settings indent space) newtype'
      <> space
      <> formatInstanceHead settings indent instanceHead
      <> newline
  CST.DeclKindSignature kindOfDeclaration labeled ->
    formatSourceToken settings indentation indent kindOfDeclaration
      <> space
      <> formatLabeledNameType settings indent labeled
  CST.DeclSignature labeled -> formatLabeledNameType settings indent labeled
  CST.DeclValue valueBindingFields ->
    formatValueBindingFields settings indent valueBindingFields
      <> newline
  CST.DeclFixity fixityFields -> formatFixityFields settings indent fixityFields <> newline
  CST.DeclForeign foreign'' import'' foreign''' ->
    formatSourceToken settings indent blank foreign''
      <> formatSourceToken settings indent space import''
      <> space
      <> formatForeign settings indent foreign'''
      <> newline
  CST.DeclRole type'' role'' name' roles -> do
    formatSourceToken settings indentation indent type''
      <> space
      <> formatSourceToken settings indentation indent role''
      <> space
      <> formatName settings indent blank name'
      <> foldMap
          ( \role' ->
              space <> formatRole settings indent role'
          )
          roles
      <> newline
  CST.DeclError e -> absurd e
  where
  indented = indent <> indentation

  lines = singleOrMultiline declaration

formatRole ::
  Settings ->
  Indent ->
  (CST.SourceToken /\ CST.Role) ->
  String
formatRole settings@{ indentation } indent (sourceToken /\ role) = do
  formatSourceToken settings indentation indent sourceToken

formatForeign ::
  Settings ->
  Indent ->
  CST.Foreign Void ->
  String
formatForeign settings indent' foreign'' = case foreign'' of
  CST.ForeignData data' labeled' -> do
    formatSourceToken settings indent' blank data'
      <> space
      <> formatLabeledNameKind settings indent' labeled'
  CST.ForeignKind kind name' -> do
    formatSourceToken settings indent' blank kind
      <> formatName settings indent' space name'
  CST.ForeignValue labeled' -> do
    formatLabeledNameType settings indent' labeled'

formatLabeledNameKind ::
  forall a.
  Settings ->
  Indent ->
  CST.Labeled
    (CST.Name a)
    (CST.Type Void) ->
  String
formatLabeledNameKind settings indent labeled =
  formatLabeled
    settings
    indent
    (formatName settings indent blank)
    (\x -> formatType settings x (singleOrMultiline labeled))
    labeled

formatFixityFields ::
  Settings ->
  Indent ->
  CST.FixityFields ->
  String
formatFixityFields settings indent { keyword: infix' /\ _, prec: precedence /\ _, operator } = do
  formatSourceToken settings indent blank infix'
    <> formatSourceToken settings indent space precedence
    <> space
    <> formatFixityOp settings indent operator

formatFixityOp ::
  Settings ->
  Indent ->
  CST.FixityOp ->
  String
formatFixityOp settings indent fixityOp' = case fixityOp' of
  CST.FixityType type'' name' as op -> do
    formatSourceToken settings indent blank type''
      <> formatQualifiedName settings indent space name'
      <> formatSourceToken settings indent space as
      <> formatName settings indent space op
  CST.FixityValue name' as op -> do
    formatQualifiedName settings indent blank name'
      <> formatSourceToken settings indent space as
      <> formatName settings indent space op

formatInstance ::
  Settings ->
  Indent ->
  CST.Instance Void ->
  String
formatInstance settings@{ indentation } indent (CST.Instance { head, body }) =
  formatInstanceHead settings indent head
    <> foldMap formatBindings body
  where
  formatBindings (whereClause /\ instanceBindings) =
    formatSourceToken settings indent space whereClause
      <> foldMap formatBinding instanceBindings

  formatBinding instanceBinding = newline <> indented <> formatInstanceBinding settings indented instanceBinding

  indented = indent <> indentation

formatInstanceHead ::
  Settings ->
  Indent ->
  CST.InstanceHead Void ->
  String
formatInstanceHead settings@{ indentation } indent'' instanceHead@{ keyword: instance''
, nameAndSeparator
, constraints: constraints'
, className
, types
} = do
  let
    lines = rangeOfInstanceHead instanceHead

    (indent /\ indent' /\ prefix) = case lines of
      MultipleLines ->
        (indent'' <> indentation <> indentation)
          /\ (indent'' <> indentation)
          /\ (newline <> indent'' <> indentation)
      SingleLine -> (indent'' /\ indent'' /\ space)

    typePrefix = case Array.last types of
      Just lastType -> case singleOrMultilineBetween className lastType of
        MultipleLines -> newline <> indent
        SingleLine -> space
      Nothing -> space
  formatSourceToken settings indent'' blank instance''
    <> foldMap
        ( \{ name: name', separator: colons } ->
            formatName settings indent'' space name'
              <> formatSourceToken settings indent'' space colons
        )
        nameAndSeparator
    <> foldMap
        ( \(constraints /\ arrow) ->
            prefix
              <> formatOneOrDelimited
                  settings
                  lines
                  indent'
                  (\t -> formatType settings indent (singleOrMultiline t) t)
                  constraints
              <> formatSourceToken settings indent' space arrow
        )
        constraints'
    <> formatQualifiedName settings indent' prefix className
    <> foldMap
        (\type'' -> typePrefix <> formatType settings indent (singleOrMultiline type'') type'')
        types

formatInstanceBinding ::
  Settings ->
  Indent ->
  CST.InstanceBinding Void ->
  String
formatInstanceBinding settings indent = case _ of
  CST.InstanceBindingSignature labeled -> do
    formatLabeledNameType settings indent labeled
  CST.InstanceBindingName valueBindingFields -> do
    formatValueBindingFields settings indent valueBindingFields

-- | This is for basics like function or value definitions
-- | ```
-- | x = true
-- | x <- [1,2,3]
-- | x | x `mod` 2 == 0 -> [1,2,3]
-- | ```
formatValueBindingFields ::
  Settings ->
  Indent ->
  CST.ValueBindingFields Void ->
  String
formatValueBindingFields settings indent { name, binders, guarded } =
  formatName settings indent blank name
    <> foldMap formatValueBinder binders
    <> formatGuarded settings indent guarded
  where
  formatValueBinder binder = space <> formatBinder settings indent binder

formatBinder ::
  Settings ->
  Indent ->
  CST.Binder Void ->
  String
formatBinder settings@{ indentation } indent' binder = case binder of
  CST.BinderArray delimited -> do
    formatArray
      settings
      lines
      indent'
      (formatBinder settings indent')
      delimited
  CST.BinderBoolean boolean _ -> do
    formatSourceToken settings indent' blank boolean
  CST.BinderChar char _ -> do
    formatSourceToken settings indent' blank char
  CST.BinderConstructor name' binders -> do
    let
      prefix = case singleOrMultiline binder of
        MultipleLines -> newline <> indent' <> indentation
        SingleLine -> space
    formatQualifiedName settings indent' blank name'
      <> foldMap
          (\binder' -> prefix <> formatBinder settings indent' binder')
          binders
  CST.BinderNamed name' at binder' -> do
    formatName settings indent' blank name'
      <> formatSourceToken settings indent' blank at
      <> formatBinder settings indent' binder'
  CST.BinderNumber negative number _ -> do
    foldMap (formatSourceToken settings indent' blank) negative
      <> formatSourceToken settings indent' blank number
  CST.BinderOp binder1 binders -> do
    formatBinder (settings { indentation = indent' }) prefix binder1 -- [TODO] check
      <> foldMap formatNamedBinder binders
    where
    indentTrick = indent' <> indentation

    indent /\ prefix = case lines of
      MultipleLines -> indentTrick /\ (newline <> indentTrick)
      SingleLine -> indent' /\ space

    formatNamedBinder (name /\ binder) =
      formatQualifiedName settings indent' prefix name
        <> prefix
        <> formatBinder settings indent binder
  CST.BinderParens wrapped -> formatParens lines settings indent' (formatBinder settings) wrapped
  CST.BinderRecord delimited -> do
    formatRecord
      settings
      indent'
      ( formatRecordLabeled
          settings
          indent'
          (formatBinder settings)
      )
      delimited
  CST.BinderString string _ -> do
    formatSourceToken settings indent' blank string
  CST.BinderTyped binder' colons type'' -> do
    let
      indentTrick = indent' <> indentation

      indent /\ prefix = case lines of
        MultipleLines -> indentTrick /\ (newline <> indentTrick)
        SingleLine -> indent' /\ space
    formatBinder settings indent' binder'
      <> formatSourceToken settings indent' space colons
      <> prefix
      <> formatType settings indent lines type''
  CST.BinderVar name' -> formatName settings indent' blank name'
  CST.BinderWildcard wildcard -> formatSourceToken settings indent' blank wildcard
  CST.BinderInt negative int _ ->
    foldMap (formatSourceToken settings indent' blank) negative
      <> formatSourceToken settings indent' blank int
  CST.BinderError x -> absurd x
  where
  lines = singleOrMultiline binder

formatGuarded ::
  Settings ->
  Indent ->
  CST.Guarded Void ->
  String
formatGuarded settings@{ indentation } indent' = case _ of
  CST.Guarded guardedExprs -> do
    let
      indent = indent' <> indentation
    foldMap
      ( \guardedExpr ->
          (newline <> indent)
            <> formatGuardedExpr settings indent guardedExpr
      )
      guardedExprs
  CST.Unconditional separator whereToken -> do
    formatSourceToken settings indent' space separator
      <> formatWhere settings indent' whereToken

formatWhere ::
  Settings ->
  Indent ->
  CST.Where Void ->
  String
formatWhere settings@{ indentation } indent' (CST.Where { expr: expr', bindings: letBindings'' }) = do
  let
    indent = indent' <> indentation
  formatExprPrefix (singleOrMultiline expr') settings indent' expr'
    <> foldMap
        ( \(where'' /\ letBindings') ->
            (newline <> indent)
              <> formatSourceToken settings indent blank where''
              <> foldMap
                  (formatLetBinding settings indent (newline <> indent) newline)
                  (NE.init letBindings')
              <> formatLetBinding
                  settings
                  indent
                  (newline <> indent)
                  blank
                  (NE.last letBindings')
        )
        letBindings''

formatLetBinding ::
  Settings ->
  Indent ->
  Prefix ->
  Suffix ->
  CST.LetBinding Void ->
  String
formatLetBinding settings@{ indentation } indent' prefix suffix = case _ of
  CST.LetBindingName valueBindingFields' -> do
    prefix
      <> formatValueBindingFields settings indent' valueBindingFields'
      <> suffix
  CST.LetBindingPattern binder' equals where'' -> do
    prefix
      <> formatBinder settings indent' binder'
      <> formatSourceToken settings indent' space equals
      <> formatWhere settings indent' where''
      <> suffix
  CST.LetBindingSignature labeled' -> do
    prefix
      <> formatLabeledNameType settings indent' labeled'
  CST.LetBindingError e -> absurd e

formatGuardedExpr ::
  Settings ->
  Indent ->
  CST.GuardedExpr Void ->
  String
formatGuardedExpr settings@{ indentation } indent' expr = case expr of
  CST.GuardedExpr { bar, patterns: patternGuards, separator, where: where'' } -> do
    let
      indent = indent' <> indentation
    formatSourceToken settings indent' blank bar
      <> space
      <> formatSeparated
          settings
          (singleOrMultilineFromRange (rangeOfSeparated rangeOfPatternGuard patternGuards))
          indent'
          space
          (formatPatternGuard settings indent)
          patternGuards
      <> space
      <> formatSourceToken settings indent' blank separator
      <> formatWhere settings indent' where''

formatPatternGuard ::
  Settings ->
  Indent ->
  CST.PatternGuard Void ->
  String
formatPatternGuard settings@{ indentation } indent patternGuard@(CST.PatternGuard { binder, expr: expr' }) = case binder of
  Just (binder' /\ arrow) ->
    formatBinder settings indent binder'
      <> formatSourceToken settings indent space arrow
      <> formatExprPrefix lines settings indent expr'
  Nothing -> formatExpr settings indent expr'
  where
  lines = singleOrMultilineFromRange (rangeOfPatternGuard patternGuard)

formatExprPrefix :: Lines -> Settings -> Indent -> CST.Expr Void -> String
formatExprPrefix lines settings@{ indentation } indent' expr = prefix <> formatExpr settings indent expr
  where
  indent = case expr of
    CST.ExprAdo _ -> indent'
    CST.ExprApp _ _ -> indent' <> indentation
    CST.ExprArray _ -> indent' <> indentation
    CST.ExprBoolean _ _ -> indent' <> indentation
    CST.ExprCase _ -> indent'
    CST.ExprChar _ _ -> indent' <> indentation
    CST.ExprConstructor _ -> indent' <> indentation
    CST.ExprDo _ -> indent'
    CST.ExprHole _ -> indent' <> indentation
    CST.ExprIdent _ -> indent' <> indentation
    CST.ExprIf _ -> indent' <> indentation
    CST.ExprInfix _ _ -> indent' <> indentation
    CST.ExprLambda _ -> indent'
    CST.ExprLet _ -> indent' <> indentation
    CST.ExprNegate _ _ -> indent' <> indentation
    CST.ExprNumber _ _ -> indent' <> indentation
    CST.ExprOp _ _ -> indent' <> indentation
    CST.ExprOpName _ -> indent' <> indentation
    CST.ExprParens _ -> indent' <> indentation
    CST.ExprRecord _ -> indent' <> indentation
    CST.ExprRecordAccessor _ -> indent' <> indentation
    CST.ExprRecordUpdate _ _ -> indent' <> indentation
    CST.ExprSection _ -> indent' <> indentation
    CST.ExprString _ _ -> indent' <> indentation
    CST.ExprTyped _ _ _ -> indent' <> indentation
    CST.ExprInt _ _ -> indent' <> indentation
    CST.ExprError e -> absurd e

  multiLine = case expr of
    CST.ExprAdo _ -> space
    CST.ExprApp _ _ -> newline <> indent
    CST.ExprArray _ -> newline <> indent
    CST.ExprBoolean _ _ -> newline <> indent
    CST.ExprCase _ -> space
    CST.ExprChar _ _ -> newline <> indent
    CST.ExprConstructor _ -> newline <> indent
    CST.ExprDo _ -> space
    CST.ExprHole _ -> newline <> indent
    CST.ExprIdent _ -> newline <> indent
    CST.ExprIf _ -> newline <> indent
    CST.ExprInfix _ _ -> newline <> indent
    CST.ExprLambda _ -> space
    CST.ExprLet _ -> newline <> indent
    CST.ExprNegate _ _ -> newline <> indent
    CST.ExprNumber _ _ -> newline <> indent
    CST.ExprOp _ _ -> newline <> indent
    CST.ExprOpName _ -> newline <> indent
    CST.ExprParens _ -> newline <> indent
    CST.ExprRecord _ -> newline <> indent
    CST.ExprRecordAccessor _ -> newline <> indent
    CST.ExprRecordUpdate _ _ -> newline <> indent
    CST.ExprSection _ -> newline <> indent
    CST.ExprString _ _ -> newline <> indent
    CST.ExprTyped _ _ _ -> newline <> indent
    CST.ExprInt _ _ -> newline <> indent
    CST.ExprError e -> absurd e

  prefix :: String
  prefix = case lines of
    MultipleLines -> multiLine
    SingleLine -> space

formatExpr ::
  Settings ->
  Indent ->
  CST.Expr Void ->
  String
formatExpr settings@{ indentation } indent'' expr'' = case expr'' of
  CST.ExprAdo adoBlock' -> formatAdoBlock lines settings indent'' adoBlock'
  CST.ExprApp expr1 expressions ->  -- [CHECK] Verify
    formatExpr settings indent'' expr1
      <> foldMap formatApp expressions
    where
    formatApp curr = (formatExprPrefix (singleOrMultilineBetween expr1 curr) settings indent'' curr)
  CST.ExprArray delimited' -> do
    let
      indent' = case lines of
        MultipleLines -> indent'' <> indentation
        SingleLine -> indent''
    formatArray
      settings
      lines
      indent''
      (formatExpr settings indent')
      delimited'
  CST.ExprBoolean boolean _ -> formatSourceToken settings indent'' blank boolean
  CST.ExprCase caseOf -> formatCaseOf lines settings indent'' caseOf
  CST.ExprChar char _ -> formatSourceToken settings indent'' blank char
  CST.ExprConstructor name -> formatQualifiedName settings indent'' blank name
  CST.ExprDo doBlock -> formatDoBlock lines settings indent'' doBlock
  CST.ExprHole hole -> formatName settings indent'' blank hole
  CST.ExprIdent name -> formatQualifiedName settings indent'' blank name
  CST.ExprIf ifThenElse -> formatIfThenElse lines settings indent'' ifThenElse
  CST.ExprInfix expr1 expressions ->
    formatExpr settings indent'' expr1
      <> prefix
      <> foldMap formatOne expressions
    where
    (indent /\ indent' /\ prefix /\ prefix') = case lines of
      MultipleLines ->
        ( (indent'' <> indentation <> indentation)
            /\ (indent'' <> indentation)
            /\ (newline <> indent'' <> indentation)
            /\ (newline <> (indent'' <> indentation) <> indentation)
        )
      SingleLine -> (indent'' /\ indent'' /\ space /\ space)

    formatOne (wrapped /\ expression) =
      formatWrapped settings indent (formatExpr settings indent') wrapped
        <> prefix'
        <> formatExpr settings indent expression
  CST.ExprLambda lambda -> do
    formatLambda lines settings indent'' lambda
  CST.ExprLet letIn -> do
    formatLetIn lines settings indent'' letIn
  CST.ExprNegate negative expression -> do
    formatSourceToken settings indent'' blank negative
      <> formatExpr settings indent'' expression
  CST.ExprNumber number _ -> do
    formatSourceToken settings indent'' blank number
  CST.ExprOp expr1 expressions -> do
    let
      indent /\ indent' /\ prefix = case lines of
        MultipleLines -> (indent'' <> indentation <> indentation) /\ (indent'' <> indentation) /\ (newline <> indent'' <> indentation)
        SingleLine -> (indent'' <> indentation) /\ indent'' /\ space
    formatExpr settings indent'' expr1
      <> foldMap
          ( \(op /\ expr) ->
              formatQualifiedName settings indent' prefix op
                <> space
                <> formatExpr settings indent expr
          )
          expressions
  CST.ExprOpName name' -> do
    formatQualifiedName settings indent'' blank name'
  CST.ExprParens wrapped' -> do
    formatParens lines settings indent'' (formatExpr settings) wrapped'
  CST.ExprRecord delimited -> do
    formatRecord
      settings
      indent''
      ( formatRecordLabeled
          settings
          indent''
          (formatExpr settings)
      )
      delimited
  CST.ExprRecordAccessor recordAccessor' -> do
    formatRecordAccessor lines settings indent'' recordAccessor'
  CST.ExprRecordUpdate expr' delimitedNonEmpty -> do
    let
      indent' /\ prefix = case lines of
        MultipleLines -> (indent'' <> indentation) /\ (newline <> indent'' <> indentation)
        SingleLine -> indent'' /\ space
    formatExpr settings indent'' expr'
      <> prefix
      <> formatRecordNonEmpty
          settings
          indent'
          (formatRecordUpdate settings indent')
          delimitedNonEmpty
  CST.ExprSection section -> formatSourceToken settings indent'' blank section
  CST.ExprString string _ -> formatSourceToken settings indent'' blank string
  CST.ExprTyped expr' colons type'' -> do
    let
      indent' /\ prefix = case lines of
        MultipleLines -> (indent'' <> indentation) /\ (newline <> indent'' <> indentation)
        SingleLine -> indent'' /\ space
    formatExpr settings indent'' expr'
      <> formatSourceToken settings indent'' space colons
      <> prefix
      <> formatType settings indent' lines type''
  CST.ExprInt int _ -> formatSourceToken settings indent'' blank int
  CST.ExprError e -> absurd e
  where
  lines = singleOrMultiline expr''

formatRecord ::
  forall a.
  Settings ->
  Indent ->
  (a -> String) ->
  CST.Delimited a ->
  String
formatRecord settings indent formatValue = case _ of
  CST.Wrapped { open, value: Nothing, close } -> do
    formatSourceToken settings indent blank open
      <> formatSourceToken settings indent blank close
  CST.Wrapped { open, value: Just record', close } ->
    formatRecordNonEmpty
      settings
      indent
      formatValue
      (CST.Wrapped { open, value: record', close })

formatRecordNonEmpty ::
  forall a.
  Settings ->
  Indent ->
  (a -> String) ->
  CST.DelimitedNonEmpty a ->
  String
formatRecordNonEmpty settings indent formatValue record' = do
  let
    (before /\ after) = case lines of
      MultipleLines -> (blank /\ blank)
      SingleLine -> (space /\ space)
  formatWrapped
    settings
    indent
    ( \separated' ->
        before
          <> formatSeparated settings lines indent space formatValue separated' -- [CHECK] is the lines correct
          <> after
    )
    record'
  where
  lines = singleOrMultiline record'

formatRecordAccessor ::
  Lines ->
  Settings ->
  Indent ->
  CST.RecordAccessor Void ->
  String
formatRecordAccessor lines settings@{ indentation } indent' recordAccessor' = case recordAccessor' of
  { expr: expr', dot, path } -> do
    let
      indentTrick = indent' <> indentation

      indent /\ prefix = case lines of
        MultipleLines -> indentTrick /\ (newline <> indentTrick)
        SingleLine -> indent' /\ blank
    formatExpr settings indent expr'
      <> prefix
      <> formatSourceToken settings indent blank dot
      <> formatSeparated
          settings
          lines -- (lines.separated SourceRange.label path)
          indent
          blank
          (formatName settings indent' blank)
          path

-- [CHECK] Verify lines stuff
formatRecordUpdate ::
  Settings ->
  Indent ->
  CST.RecordUpdate Void ->
  String
formatRecordUpdate settings@{ indentation } indent' recordUpdate' = case recordUpdate' of
  CST.RecordUpdateBranch label' delimitedNonEmpty' -> do
    let
      lines = singleOrMultiline delimitedNonEmpty'

      indentTrick = indent' <> indentation

      indent /\ prefix = case lines of
        MultipleLines -> indentTrick /\ (newline <> indentTrick)
        SingleLine -> indent' /\ space
    formatName settings indent' blank label'
      <> prefix
      <> formatRecordNonEmpty
          settings
          indent
          (formatRecordUpdate settings indent)
          delimitedNonEmpty'
  CST.RecordUpdateLeaf label' equals expr' -> do
    let
      lines = singleOrMultiline expr'

      indentTrick = indent' <> indentation

      indent /\ prefix = case lines of
        MultipleLines -> indentTrick /\ (newline <> indentTrick)
        SingleLine -> indent' /\ space
    formatName settings indent' blank label'
      <> formatSourceToken settings indent' space equals
      <> prefix
      <> formatExpr settings indent expr'

formatRecordLabeled ::
  forall a.
  RangeOf a =>
  Settings ->
  Indent ->
  (Indent -> a -> String) ->
  CST.RecordLabeled a ->
  String
formatRecordLabeled settings@{ indentation } indent' f recordLabeled' = case recordLabeled' of
  CST.RecordPun name' -> do
    formatName settings indent' blank name'
  CST.RecordField label' colon a -> do
    let
      indent /\ prefix = case singleOrMultilineBetween label' a of
        MultipleLines -> (indent' <> indentation <> indentation) /\ (newline <> indent' <> indentation <> indentation)
        SingleLine -> indent' /\ space
    formatName settings indent' blank label'
      <> formatSourceToken settings indent' blank colon
      <> prefix
      <> f indent a

formatLetIn ::
  Lines ->
  Settings ->
  Indent ->
  CST.LetIn Void ->
  String
formatLetIn lines settings@{ indentation } indent' letIn' = case letIn' of
  { keyword: let'
  , bindings: letBindings
  , in: in'
  , body: bodyExpression
  } -> do
    let
      inPrefix /\ indent /\ prefix = case lines of
        MultipleLines -> (newline <> indent') /\ (indent' <> indentation) /\ (newline <> indent' <> indentation)
        SingleLine -> space /\ indent' /\ space
    formatSourceToken settings indent' blank let'
      <> foldMap
          (formatLetBinding settings indent prefix newline)
          (NE.init letBindings)
      <> formatLetBinding settings indent prefix blank (NE.last letBindings)
      <> inPrefix
      <> formatSourceToken settings indent' blank in'
      <> prefix
      <> formatExpr settings indent bodyExpression

formatLambda ::
  Lines ->
  Settings ->
  Indent ->
  CST.Lambda Void ->
  String
formatLambda lines settings@{ indentation } indent' lambda' = case lambda' of
  { symbol: reverseSolidus, binders, arrow, body: expr' } -> do
    formatSourceToken settings indent' blank reverseSolidus
      <> foldMap (\binder' -> formatBinder settings indent' binder' <> space) binders
      <> formatSourceToken settings indent' blank arrow
      <> formatExprPrefix lines settings indent' expr'

formatExprPrefixElseIf ::
  Lines ->
  Settings ->
  Indent ->
  CST.Expr Void ->
  String
formatExprPrefixElseIf lines settings@{ indentation } indent expr' = case expr' of
  CST.ExprAdo _ -> formatExprPrefix lines settings indent expr'
  CST.ExprApp _ _ -> formatExprPrefix lines settings indent expr'
  CST.ExprArray _ -> formatExprPrefix lines settings indent expr'
  CST.ExprBoolean _ _ -> formatExprPrefix lines settings indent expr'
  CST.ExprCase _ -> formatExprPrefix lines settings indent expr'
  CST.ExprChar _ _ -> formatExprPrefix lines settings indent expr'
  CST.ExprConstructor _ -> formatExprPrefix lines settings indent expr'
  CST.ExprDo _ -> formatExprPrefix lines settings indent expr'
  CST.ExprHole _ -> formatExprPrefix lines settings indent expr'
  CST.ExprIdent _ -> formatExprPrefix lines settings indent expr'
  CST.ExprIf _ -> space <> formatExpr settings indent expr'
  CST.ExprInfix _ _ -> formatExprPrefix lines settings indent expr'
  CST.ExprLambda _ -> formatExprPrefix lines settings indent expr'
  CST.ExprLet _ -> formatExprPrefix lines settings indent expr'
  CST.ExprNegate _ _ -> formatExprPrefix lines settings indent expr'
  CST.ExprNumber _ _ -> formatExprPrefix lines settings indent expr'
  CST.ExprOp _ _ -> formatExprPrefix lines settings indent expr'
  CST.ExprOpName _ -> formatExprPrefix lines settings indent expr'
  CST.ExprParens _ -> formatExprPrefix lines settings indent expr'
  CST.ExprRecord _ -> formatExprPrefix lines settings indent expr'
  CST.ExprRecordAccessor _ -> formatExprPrefix lines settings indent expr'
  CST.ExprRecordUpdate _ _ -> formatExprPrefix lines settings indent expr'
  CST.ExprSection _ -> formatExprPrefix lines settings indent expr'
  CST.ExprString _ _ -> formatExprPrefix lines settings indent expr'
  CST.ExprTyped _ _ _ -> formatExprPrefix lines settings indent expr'
  CST.ExprInt _ _ -> formatExprPrefix lines settings indent expr'
  CST.ExprError e -> absurd e

formatIfThenElse ::
  Lines ->
  Settings ->
  Indent ->
  CST.IfThenElse Void ->
  String
formatIfThenElse lines settings@{ indentation } indent { keyword: if', cond, then: then', true: true', else: else', false: false' } = do
  formatSourceToken settings indent blank if'
    <> space
    <> formatExpr settings indent cond
    <> space
    <> formatSourceToken settings indent blank then'
    <> formatExprPrefix lines settings indent true'
    <> prefix
    <> formatSourceToken settings indent blank else'
    <> formatExprPrefixElseIf lines settings indent false'
  where
  prefix = case lines of
    MultipleLines -> newline <> indent
    SingleLine -> space

formatDoBlock ::
  Lines ->
  Settings ->
  Indent ->
  CST.DoBlock Void ->
  String
formatDoBlock lines settings@{ indentation } indent' doBlock' = case doBlock' of
  { keyword: do', statements: doStatements } -> do
    let
      indentTrick = indent' <> indentation

      indent /\ prefix = case lines of
        MultipleLines -> indentTrick /\ (newline <> indentTrick)
        SingleLine -> indent' /\ space
    formatSourceToken settings indent' blank do'
      <> foldMap
          ( \doStatement' ->
              prefix
                <> formatDoStatement settings indent doStatement'
          )
          doStatements

formatDoStatement ::
  Settings ->
  Indent ->
  CST.DoStatement Void ->
  String
formatDoStatement settings@{ indentation } indent' doStatement' = case doStatement' of
  CST.DoBind binder' arrow expr' -> do
    formatBinder settings indent' binder'
      <> formatSourceToken settings indent' space arrow
      <> formatExprPrefix (singleOrMultiline doStatement') settings indent' expr'
  CST.DoDiscard expr' -> do
    formatExpr settings indent' expr'
  CST.DoLet let' letBindings -> do
    let
      indent = indent' <> indentation
    formatSourceToken settings indent' blank let'
      <> foldMap
          (formatLetBinding settings indent (newline <> indent) newline)
          (NE.init letBindings)
      <> formatLetBinding
          settings
          indent
          (newline <> indent)
          blank
          (NE.last letBindings)
  CST.DoError e -> absurd e

formatCaseOf ::
  Lines ->
  Settings ->
  Indent ->
  CST.CaseOf Void ->
  String
formatCaseOf lines settings@{ indentation } indent' caseOf' = case caseOf' of
  { keyword: case', head, of: of', branches } -> do
    let
      indent /\ prefix = case lines of
        MultipleLines -> (indent' <> indentation) /\ (newline <> indent' <> indentation)
        SingleLine -> indent' /\ space
    formatSourceToken settings indent' blank case'
      <> space
      <> formatSeparated
          settings
          (singleOrMultiline head)
          indent
          space
          (formatExpr settings indent)
          head
      <> space
      <> formatSourceToken settings indent' blank of'
      <> foldMap
          ( \(binders /\ guarded) ->
              prefix
                <> formatSeparated
                    settings
                    (singleOrMultiline binders)
                    indent
                    space
                    (formatBinder settings indent)
                    binders
                <> formatGuarded settings indent guarded
          )
          branches

formatArray ::
  forall a.
  RangeOf a =>
  Settings ->
  Lines ->
  Indent ->
  (a -> String) ->
  CST.Delimited a ->
  String
formatArray settings lines indent formatValue = case _ of
  -- Empty array
  CST.Wrapped { open, value: Nothing, close } -> do
    formatSourceToken settings indent blank open
      <> formatSourceToken settings indent blank close
  CST.Wrapped { open, value: Just array, close } ->
    formatArrayNonEmpty
      settings
      lines
      indent
      formatValue
      (CST.Wrapped { open, value: array, close })

formatArrayNonEmpty ::
  forall a.
  RangeOf a =>
  Settings ->
  Lines ->
  Indent ->
  (a -> String) ->
  CST.DelimitedNonEmpty a ->
  String
formatArrayNonEmpty settings lines indent formatValue = formatWrapped settings indent formatOne
  where
  before /\ after = case lines of
    MultipleLines -> blank /\ blank
    SingleLine -> space /\ space

  formatOne (separated :: CST.Separated a) =
    before
      <> formatSeparated settings (singleOrMultiline separated) indent space formatValue separated
      <> after

formatAdoBlock ::
  Lines ->
  Settings ->
  Indent ->
  CST.AdoBlock Void ->
  String
formatAdoBlock lines settings@{ indentation } indent' { keyword, statements: doStatements, in: in', result: expr' } = do
  let
    { indent, prefix } = case lines of
      MultipleLines -> do
        let
          indent = indent' <> indentation
        { indent, prefix: newline <> indent }
      SingleLine -> { indent: indent', prefix: space }
  formatSourceToken settings indent' blank keyword
    <> foldMap
        (\doStatement' -> prefix <> formatDoStatement settings indent doStatement')
        doStatements
    <> prefix
    <> formatSourceToken settings indent' blank in'
    <> space
    <> formatExpr settings indent expr'

formatLabeledNameType ::
  forall a.
  Settings ->
  Indent ->
  CST.Labeled (CST.Name a) (CST.Type Void) ->
  String
formatLabeledNameType settings indent labeled =
  formatLabeled
    settings
    indent
    (formatName settings indent blank)
    (\x -> formatType settings x (singleOrMultiline labeled))
    labeled

formatClassHead ::
  Lines ->
  Settings ->
  Indent ->
  CST.ClassHead Void ->
  String
formatClassHead lines settings@{ indentation } indent { keyword, super, name, vars, fundeps } = do
  formatSourceToken settings indented blank keyword
    <> foldMap formatSuper super
    <> formatName settings indent space name
    <> foldMap formatTypeVariableBinding vars
    <> foldMap formatFunctionalDependency fundeps
  where
  { indented, prefix } = case lines of
    MultipleLines -> do
      let
        indented = indent <> indentation
      { indented, prefix: newline <> indented }
    SingleLine -> { indented: indent, prefix: space }

  formatTypeVariableBinding binding = space <> formatTypeVarBinding settings indent binding

  formatFunctionalDependency (bar /\ classFundeps) =
    formatSourceToken settings indent space bar
      <> space
      <> formatSeparated
          settings
          lines
          indent
          space
          (formatClassFundep settings indent)
          classFundeps

  formatSuper (constraints /\ arrow) =
    prefix
      <> formatOneOrDelimited
          settings
          lines
          indented
          (formatType settings indent lines)
          constraints
      <> formatSourceToken settings indent space arrow

formatClassFundep ::
  Settings ->
  Indent ->
  CST.ClassFundep ->
  String
formatClassFundep settings indent = case _ of
  CST.FundepDetermined arrow names ->
    formatSourceToken settings indent blank arrow
      <> foldMap (formatName settings indent space) names
  CST.FundepDetermines names arrow moreNames -> do
    foldMap (\name -> formatName settings indent blank name <> space) names
      <> formatSourceToken settings indent blank arrow
      <> foldMap (formatName settings indent space) moreNames

formatOneOrDelimited ::
  forall a.
  RangeOf a =>
  Settings ->
  Lines ->
  Indent ->
  (a -> String) ->
  CST.OneOrDelimited a ->
  String
formatOneOrDelimited settings lines indent formatValue oneOrDelimited = case oneOrDelimited of
  CST.One a -> formatValue a
  CST.Many delimitedNonEmpty -> formatDelimitedNonEmpty settings lines indent formatValue delimitedNonEmpty

formatDelimitedNonEmpty ::
  forall a.
  RangeOf a =>
  Settings ->
  Lines ->
  Indent ->
  (a -> String) ->
  CST.DelimitedNonEmpty a ->
  String
formatDelimitedNonEmpty settings lines indent formatValue = formatWrapped settings indent (formatSeparated settings lines indent space formatValue)

formatDataConstructor ::
  Settings ->
  Indent ->
  CST.DataCtor Void ->
  String
formatDataConstructor settings@{ indentation } indent constructor@(CST.DataCtor { name, fields }) =
  space
    <> formatName settings indent blank name
    <> foldMap formatField fields
  where
  lines = singleOrMultiline constructor

  { indented, prefix } = case lines of
    MultipleLines -> do
      let
        indented = indent <> indentation
      { indented, prefix: newline <> indented }
    SingleLine -> { indented: indent, prefix: space }

  formatField typo = prefix <> formatType settings indented lines typo

-- | Formats the `data Maybe a` part of `data Maybe a = Just a | Nothing`
formatDataHead :: Settings -> Indent -> CST.DataHead Void -> String
formatDataHead settings indent { keyword, name, vars } =
  formatSourceToken settings indent blank keyword
    <> formatName settings indent space name
    <> formatTypeVarBindings settings indent vars

formatTypeVarBindings ::
  Settings ->
  Indent ->
  Array (CST.TypeVarBinding Void) ->
  String
formatTypeVarBindings settings indent = foldMap (pure space <> formatTypeVarBinding settings indent)

formatTypeVarBinding ::
  Settings ->
  Indent ->
  CST.TypeVarBinding Void ->
  String
formatTypeVarBinding settings indent = case _ of
  CST.TypeVarName name -> formatName settings indent blank name
  CST.TypeVarKinded wrapped -> formatWrapped settings indent (formatLabeledName settings indent) wrapped

formatType ::
  Settings ->
  Indent ->
  Lines ->
  CST.Type Void ->
  String
formatType settings@{ indentation } indent lines t = case t of
  CST.TypeApp typo types ->
    formatType settings indent (singleOrMultiline typo) typo
      <> foldMap formatTypeWithPredecessor
          (Array.zip typesArray (typo Array.: typesArray))
    where
    typesArray = NE.toArray types

    formatTypeWithPredecessor (curr /\ pred) = prefix <> formatType settings indented (singleOrMultiline curr) curr
      where
      { indented, prefix } = case singleOrMultilineBetween curr pred of
        MultipleLines -> { indented: indent <> indentation, prefix: newline <> indent <> indentation }
        SingleLine -> { indented: indent, prefix: space }
  CST.TypeArrow t1 arrow t2 ->
    formatType settings indent (singleOrMultiline t1) t1
      <> space
      <> formatSourceToken settings indent blank arrow
      <> prefix
      <> formatType settings indent (singleOrMultiline t2) t2
    where
    prefix = case lines of
      MultipleLines -> newline <> indent
      SingleLine -> space
  CST.TypeArrowName arrowName -> formatSourceToken settings indent blank arrowName
  CST.TypeVar name -> formatName settings indent blank name
  CST.TypeConstructor qualifiedName -> formatQualifiedName settings indent blank qualifiedName
  CST.TypeWildcard wildcard -> formatSourceToken settings indent blank wildcard
  CST.TypeHole name -> formatName settings indent blank name
  CST.TypeString typeString _ -> formatSourceToken settings indent blank typeString
  CST.TypeRow wrappedRow -> formatWrappedRow settings indent wrappedRow
  CST.TypeRecord wrappedRow -> formatWrappedRow settings indent wrappedRow
  CST.TypeForall forAll typeVarBindings dot tailType ->
    formatSourceToken settings indent blank forAll
      <> formatTypeVarBindings settings indent (NE.toArray typeVarBindings)
      <> formatSourceToken settings indent blank dot
      <> prefix
      <> formatType settings indent lines tailType
    where
    prefix = case lines of
      MultipleLines -> newline <> indent
      SingleLine -> space
  CST.TypeKinded typo colons kind ->
    formatType settings indent lines typo
      <> space
      <> formatSourceToken settings indent blank colons
      <> prefix
      <> formatType settings indented lines kind
    where
    { indented, prefix } = case lines of
      MultipleLines -> { indented: indent <> indentation, prefix: newline <> indent }
      SingleLine -> { indented: indent, prefix: space }
  CST.TypeOp typo types ->
    formatType settings indent (singleOrMultiline typo) typo
      <> foldMap formatOp types -- [TODO] Verify
    where
    formatOp (op /\ anotherType) =
      formatQualifiedName settings indented prefix op
        <> prefix
        <> formatType settings indented (singleOrMultiline anotherType) anotherType
      where
      { indented, prefix } = case lines of
        MultipleLines -> { indented: indent <> indentation, prefix: newline <> indent }
        SingleLine -> { indented: indent, prefix: space }
  CST.TypeOpName op -> formatQualifiedName settings indent blank op
  CST.TypeConstrained constraint arrow typo ->
    formatType settings indent lines constraint
      <> space
      <> formatSourceToken settings indent blank arrow
      <> prefix
      <> formatType settings indent lines typo
    where
    prefix = case lines of
      MultipleLines -> newline <> indent
      SingleLine -> space
  CST.TypeParens wrapped ->
    formatParens lines settings indent
      ( \x wrappedType ->
          formatType settings x (singleOrMultiline wrappedType) wrappedType
      )
      wrapped
  CST.TypeUnaryRow sourceToken typo ->
    formatSourceToken settings indent blank sourceToken
      <> prefix
      <> formatType settings indent lines typo
    where
    prefix = case lines of
      MultipleLines -> newline <> indent
      SingleLine -> space
  CST.TypeError e -> absurd e

formatLabeled ::
  forall a b.
  RangeOf a =>
  RangeOf b =>
  Settings ->
  Indent ->
  (a -> String) ->
  (Indent -> b -> String) ->
  CST.Labeled a b ->
  String
formatLabeled settings@{ indentation } indent' formatLabel formatValue labeled@(CST.Labeled { label, separator, value }) =
  formatLabel label
    <> formatSourceToken settings indent space separator
    <> prefix
    <> formatValue indent value
  where
  { indent, prefix } = case singleOrMultiline labeled of
    MultipleLines -> do
      { indent: indent' <> indentation
      , prefix: newline <> indent' <> indentation
      }
    SingleLine -> { indent: indent', prefix: space }

formatLabeledName ::
  forall n.
  RangeOf (CST.Name n) =>
  Settings ->
  Indent ->
  CST.Labeled (CST.Name n) (CST.Type Void) ->
  String
formatLabeledName settings@{ indentation } indent labeledName =
  formatLabeled
    settings
    indent
    (formatName settings indent blank)
    (\x -> formatType settings x (singleOrMultiline labeledName))
    labeledName

formatParens ::
  forall a.
  Lines ->
  Settings ->
  Indent ->
  (Indent -> a -> String) ->
  CST.Wrapped a ->
  String
formatParens lines settings@{ indentation } indent formatValue wrapped = do
  formatWrapped settings indent (formatValue indented) wrapped
  where
  indented = case lines of
    MultipleLines -> indent <> indentation
    SingleLine -> indent

formatRow ::
  Lines ->
  Settings ->
  Indent ->
  CST.Row Void ->
  String
formatRow lines settings@{ indentation } indent (CST.Row { labels, tail }) = case labels, tail of
  Nothing, Nothing -> blank
  Just ls, Nothing ->
    before
      <> formatSeparated settings lines indent space f ls
      <> after
    where
    { before, indented, after } = case lines of
      MultipleLines -> { before: blank, indented: indent <> indentation, after: blank }
      SingleLine -> { before: space, indented: indent, after: space }

    f = formatLabeledName settings indented
  Nothing, Just (bar /\ tailType) -> do
    before
      <> formatSourceToken settings indent blank bar
      <> space
      <> formatType settings indent lines tailType
      <> after
    where
    { before, after } = case lines of
      MultipleLines -> { before: newline <> indent, after: blank }
      SingleLine -> { before: space, after: space }
  Just ls, Just (bar /\ tailType) -> do
    before
      <> formatSeparated settings lines indent space f ls
      <> prefix
      <> formatSourceToken settings indent blank bar
      <> space
      <> formatType settings indent lines tailType
      <> after
    where
    { before, indented, after, prefix } = case lines of
      MultipleLines -> { before: blank, indented: indent <> indentation, after: blank, prefix: newline <> indent }
      SingleLine -> { before: space, indented: indent, after: space, prefix: space }

    f = formatLabeledName settings indented

formatSeparated ::
  forall a.
  Settings ->
  Lines ->
  Indent ->
  Prefix ->
  (a -> String) ->
  CST.Separated a ->
  String
formatSeparated settings lines indent prefix' formatValue (CST.Separated { head, tail }) = formatValue head <> foldMap go tail
  where
  prefix = case lines of
    MultipleLines -> newline <> indent
    SingleLine -> blank

  go :: CST.SourceToken /\ a -> String
  go (separator /\ value) =
    prefix
      <> formatSourceToken settings indent blank separator
      <> prefix'
      <> formatValue value

formatName :: forall a. Settings -> Indent -> Prefix -> CST.Name a -> String
formatName settings indent prefix (CST.Name { token }) = 
  formatSourceToken settings indent prefix token

formatQualifiedName :: forall a. Settings -> Indent -> Prefix -> CST.QualifiedName a -> String
formatQualifiedName settings indent prefix (CST.QualifiedName { token }) = formatSourceToken settings indent prefix token

formatSourceToken ::
  Settings ->
  Indent ->
  Prefix ->
  CST.SourceToken ->
  String
formatSourceToken settings indent prefix { leadingComments, trailingComments, value } =
  formatCommentsLeading indent prefix leadingComments
    <> prefix
    <> print value
    <> formatCommentsTrailing prefix trailingComments
  where
    print = case settings.sourceStyle of
      Nothing -> Print.printToken
      Just style -> printWithStyle style
    printWithStyle style v = case v of 
      CST.TokLeftArrow _ ->
        case style of
          CST.ASCII -> "<-"
          CST.Unicode -> "←"
      CST.TokRightArrow _ ->
        case style of
          CST.ASCII -> "->"
          CST.Unicode -> "→"
      CST.TokRightFatArrow _ ->
        case style of
          CST.ASCII -> "=>"
          CST.Unicode -> "⇒"
      CST.TokDoubleColon _ ->
        case style of
          CST.ASCII -> "::"
          CST.Unicode -> "∷"
      CST.TokForall _ ->
        case style of
          CST.ASCII -> "forall"
          CST.Unicode -> "∀"
      CST.TokSymbolArrow _ ->
        case style of
          CST.ASCII -> "(->)"
          CST.Unicode -> "(→)"
      _ -> Print.printToken v

formatCommentLeading ::
  Indent ->
  Prefix ->
  CST.Comment CST.LineFeed ->
  String
formatCommentLeading indent prefix = case _ of
  CST.Comment comment -> do
    prefix <> comment <> newline <> indent
  CST.Line _ _ -> blank
  CST.Space _ -> blank

formatCommentTrailing ::
  Prefix ->
  CST.Comment Void ->
  String
formatCommentTrailing prefix comment'' = case comment'' of
  CST.Comment comment' -> prefix <> space <> comment'
  CST.Line _ _ -> blank
  CST.Space _ -> blank

formatCommentsLeading ::
  Indent ->
  Prefix ->
  Array (CST.Comment CST.LineFeed) ->
  String
formatCommentsLeading indent prefix commentsLeading' = case commentsLeading' of
  [] -> blank
  _ -> foldMap (formatCommentLeading indent prefix) commentsLeading'

formatCommentsTrailing ::
  Prefix ->
  Array (CST.Comment Void) ->
  String
formatCommentsTrailing prefix commentsTrailing' = case commentsTrailing' of
  [] -> blank
  _ -> foldMap (formatCommentTrailing prefix) commentsTrailing'

formatCommentsTrailingModule ::
  Array (CST.Comment CST.LineFeed) ->
  String
formatCommentsTrailingModule commentsTrailing' = do
  let
    comments = map formatComment commentsTrailing'
  case NE.fromArray (Array.catMaybes comments) of
    Nothing -> blank
    Just comments -> newline <> fold (NE.intersperse newline comments) <> newline

formatComment :: forall lf. CST.Comment lf -> Maybe String
formatComment = case _ of
  CST.Comment comment -> Just comment
  CST.Space _ -> Nothing
  CST.Line _ _ -> Nothing

formatWrapped ::
  forall a.
  Settings ->
  Indent ->
  (a -> String) ->
  CST.Wrapped a ->
  String
formatWrapped settings indent formatValue wrapped@(CST.Wrapped { open, value, close }) =
  formatSourceToken settings indent blank open
    <> before
    <> formatValue value
    <> after
    <> formatSourceToken settings indent blank close
  where
  { before, after } = case singleOrMultiline wrapped of
    MultipleLines -> { before: space, after: newline <> indent }
    SingleLine -> { before: blank, after: blank }

formatWrappedRow ::
  Settings ->
  Indent ->
  CST.Wrapped (CST.Row Void) ->
  String
formatWrappedRow settings@{ indentation } indent wrapped@(CST.Wrapped { open, value: row, close }) =
  formatSourceToken settings indent blank open
    <> before
    <> formatRow lines settings indent row
    <> after
    <> formatSourceToken settings indent blank close
  where
  lines = singleOrMultiline wrapped

  before /\ after = case row, lines of
    CST.Row { labels: Just _ }, MultipleLines -> space /\ (newline <> indent)
    CST.Row { labels: Nothing }, MultipleLines -> blank /\ (newline <> indent)
    _, SingleLine -> blank /\ blank

type Indentation
  = String -- [TODO] Use Numbers

type Indent
  = String

type Prefix
  = String

type Suffix
  = String

newline :: String
newline = "\n"

blank :: String
blank = ""

space :: String
space = " "

