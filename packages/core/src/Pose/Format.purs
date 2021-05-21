module Pose.Format (format) where

import Prelude
import Data.Array as Array
import Data.Array.NonEmpty as NE
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Foldable (fold, foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup.Foldable (intercalateMap)
import Data.Tuple.Nested (type (/\), (/\))
import Pose.Format.CommentHack (unwrapTrailingComments, wrapTrailingComment)
import Pose.Format.Indentations (Indentation, Lines(..), Prefix, Suffix, blank, hasNonWhitespaceTrailingComment, isPrecededByBlankLines, isPrecededByNewline, newline, precedingEmptyLines, rangeOfInstanceHead, rangeOfPatternGuard, rangeOfSeparated, singleOrMultiline, singleOrMultilineBetween, singleOrMultilineBetweenSourceRanges, singleOrMultilineFromRange, sourceTokenIsPrecededByNewline, space, unsafeFirstTokenOf)
import Pose.Settings (Settings)
import PureScript.CST.Print as Print
import PureScript.CST.Range (class RangeOf, rangeOf)
import PureScript.CST.Types as CST

format ∷ Settings → CST.Module Void → String
format settings = unwrapTrailingComments <<< formatModule settings

formatAdoBlock ∷
  Lines →
  Settings →
  Indentation →
  CST.AdoBlock Void →
  String
formatAdoBlock lines settings@{ tabSize } indent { keyword, statements: doStatements, in: inKeyword, result } =
  formatSourceToken settings indent blank keyword
    <> foldMap formatOne doStatements
    <> prefix
    <> formatSourceToken settings indent blank inKeyword
    <> space
    <> formatExpr settings indentNext result
  where
  { indentNext, prefix } = case lines of
    MultipleLines → do
      { indentNext: indent <> tabSize, prefix: newline <> indent <> tabSize }
    SingleLine → { indentNext: indent, prefix: space }

  formatOne doStatement =
    prefix <> formatDoStatement settings indentNext doStatement

formatArray ∷
  ∀ a.
  RangeOf a ⇒
  Settings →
  Lines →
  Indentation →
  (a → String) →
  CST.Delimited a →
  String
formatArray settings lines indent formatValue = case _ of
  -- Empty array
  CST.Wrapped { open, value: Nothing, close } → do
    formatSourceToken settings indent blank open
      <> formatSourceToken settings indent blank close
  CST.Wrapped { open, value: Just array, close } →
    formatArrayNonEmpty
      settings
      lines
      indent
      formatValue
      (CST.Wrapped { open, value: array, close })

formatArrayNonEmpty ∷
  ∀ a.
  RangeOf a ⇒
  Settings →
  Lines →
  Indentation →
  (a → String) →
  CST.DelimitedNonEmpty a →
  String
formatArrayNonEmpty settings lines indent formatValue = formatWrapped settings indent formatOne
  where
  indentation = case lines of
    MultipleLines → blank
    SingleLine → space

  formatOne (separated ∷ CST.Separated a) =
    indentation
      <> formatSeparated settings (singleOrMultiline separated) indent space formatValue separated
      <> indentation

formatBinder ∷
  Settings →
  Indentation →
  CST.Binder Void →
  String
formatBinder settings@{ tabSize } indent binder = case binder of
  CST.BinderArray delimited →
    formatArray
      settings
      lines
      indent
      (formatBinder settings indent)
      delimited

  CST.BinderBoolean boolean _ →
    formatSourceToken settings indent blank boolean

  CST.BinderChar char _ →
    formatSourceToken settings indent blank char

  CST.BinderConstructor name binders → do
    formatQualifiedName settings indent blank name
      <> foldMap formatBinderConstructor binders
    where
    prefix = case lines of
      MultipleLines → newline <> indent <> tabSize
      SingleLine → space
    formatBinderConstructor binderConstructor = prefix <> formatBinder settings (indent <> tabSize) binderConstructor

  -- with @ sign
  CST.BinderNamed name at binderNamed →
    formatName settings indent blank name
      <> formatSourceToken settings indent blank at
      <> atSuffix
      <> formatBinder settings binderIndent binderNamed
    where
    innerLines = singleOrMultilineBetween name binderNamed
    atSuffix = case innerLines of
      SingleLine → blank
      MultipleLines → newline <> indent <> tabSize
    binderIndent = case innerLines of
      SingleLine → indent
      MultipleLines → indent <> tabSize

  CST.BinderNumber negative number _ →
    foldMap (formatSourceToken settings indent blank) negative
      <> formatSourceToken settings indent blank number

  CST.BinderOp binder1 binders →
    formatBinder settings indentFirst binder1
      <> foldMap formatNamedBinder binders
    where
    indented = indent <> tabSize

    indentFirst = case lines of
      MultipleLines → indent
      SingleLine → indent

    { indentNext, prefix } = case lines of
      MultipleLines → { indentNext: indented, prefix: newline <> indented }
      SingleLine → { indentNext: indent, prefix: space }

    formatNamedBinder (name /\ innerBinder) =
      formatQualifiedName settings indent prefix name
        <> prefix
        <> formatBinder settings indentNext innerBinder

  CST.BinderParens wrapped → do
    formatParens lines settings indent (formatBinder settings) wrapped

  CST.BinderRecord delimited → do
    formatRecord
      settings
      indent
      ( formatRecordLabeled
          settings
          indent
          (formatBinder settings)
      )
      delimited

  CST.BinderString string _ → do
    formatSourceToken settings indent blank string

  CST.BinderTyped binderTyped colons typo → do
    formatBinder settings indent binderTyped
      <> formatSourceToken settings indent space colons
      <> prefix
      <> formatType settings indentNext lines typo
    where
    indented = indent <> tabSize
    { indentNext, prefix } = case lines of
      MultipleLines → { indentNext: indented, prefix: newline <> indented }
      SingleLine → { indentNext: indent, prefix: space }

  CST.BinderVar name →
    formatName settings indent blank name

  CST.BinderWildcard wildcard →
    formatSourceToken settings indent blank wildcard

  CST.BinderInt negative int _ →
    foldMap (formatSourceToken settings indent blank) negative
      <> formatSourceToken settings indent blank int

  CST.BinderError x → absurd x
  where
  lines = singleOrMultiline binder

formatCaseOf ∷
  Lines →
  Settings →
  Indentation →
  CST.CaseOf Void →
  String
formatCaseOf lines settings@{ tabSize } indent { keyword: caseKeyword, head, of: ofKeyword, branches } = do
  formatSourceToken settings indent blank caseKeyword
    <> space
    <> formatSeparated
        settings
        (singleOrMultiline head)
        indentNext
        space
        (formatExpr settings indentNext)
        head
    <> space
    <> formatSourceToken settings indent blank ofKeyword
    <> foldMap formatBranch branches
  where
  { indentNext, branchPrefix } = case lines of
    MultipleLines → { indentNext: indent <> tabSize, branchPrefix: newline <> indent <> tabSize }
    SingleLine → { indentNext: indent, branchPrefix: space }

  formatBranch ∷ (CST.Separated (CST.Binder Void)) /\ (CST.Guarded Void) → String
  formatBranch (binders /\ guarded) =
    preservedNewlineBeforeBranch
      <> branchPrefix
      <> formatSeparated
          settings
          (singleOrMultiline binders)
          indentNext
          space
          (formatBinder settings indentNext)
          binders
      <> formatGuarded
          settings (singleOrMultilineBetween binders guarded) indentNext guarded
    where
    preservedNewlineBeforeBranch =
      if isPrecededByBlankLines binders then
        newline
      else
        blank

formatClassHead ∷
  Lines →
  Settings →
  Indentation →
  CST.ClassHead Void →
  String
formatClassHead lines settings@{ tabSize } indent { keyword, super, name, vars, fundeps } =
  formatSourceToken settings indent blank keyword
    <> foldMap formatSuper super
    <> formatName settings indent space name
    <> foldMap formatTypeVariableBinding vars
    <> foldMap formatFunctionalDependency fundeps
  where
  indented = indent <> tabSize
  { indentNext, prefix } = case lines of
    MultipleLines →
      { indentNext: indented, prefix: newline <> indented }
    SingleLine → { indentNext: indent, prefix: space }

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
          indentNext
          (formatType settings indent lines)
          constraints
      <> formatSourceToken settings indent space arrow

formatClassFundep ∷
  Settings →
  Indentation →
  CST.ClassFundep →
  String
formatClassFundep settings indent = case _ of
  CST.FundepDetermined arrow names →
    formatSourceToken settings indent blank arrow
      <> foldMap (formatName settings indent space) names
  CST.FundepDetermines names arrow moreNames → do
    foldMap (\name → formatName settings indent blank name <> space) names
      <> formatSourceToken settings indent blank arrow
      <> foldMap (formatName settings indent space) moreNames

formatCommentsLeading ∷
  Indentation →
  Prefix →
  Array (CST.Comment CST.LineFeed) →
  String
formatCommentsLeading indent prefix commentsLeading = case commentsLeading of
  [] → blank
  _ → foldMap (formatCommentLeading indent prefix) commentsLeading

formatCommentLeading ∷
  Indentation →
  Prefix →
  CST.Comment CST.LineFeed →
  String
formatCommentLeading indent prefix = case _ of
  CST.Comment comment → prefix <> comment <> newline <> indent
  CST.Line _ _ → blank
  CST.Space _ → blank

formatCommentsTrailing ∷
  Prefix →
  Array (CST.Comment Void) →
  String
formatCommentsTrailing prefix commentsTrailing = case commentsTrailing of
  [] → blank
  _ → foldMap (formatCommentTrailing prefix) commentsTrailing

formatCommentTrailing ∷
  Prefix →
  CST.Comment Void →
  String
formatCommentTrailing prefix = case _ of
  CST.Comment comment → prefix <> space <> comment
  CST.Line _ _ → blank
  CST.Space _ → blank

formatCommentsTrailingModule ∷
  Array (CST.Comment CST.LineFeed) →
  String
formatCommentsTrailingModule commentsTrailing = do
  let
    formattedComments = map formatComment commentsTrailing
  case NE.fromArray (Array.catMaybes formattedComments) of
    Nothing → blank
    Just comments →
      newline <> fold (NE.intersperse newline comments) <> newline
  where
  formatComment ∷ ∀ lf. CST.Comment lf → Maybe String
  formatComment = case _ of
    CST.Comment comment → Just comment
    CST.Space _ → Nothing
    CST.Line _ _ → Nothing

formatDataConstructor ∷
  Settings →
  Indentation →
  CST.DataCtor Void →
  String
formatDataConstructor settings@{ tabSize } indent constructor@(CST.DataCtor { name, fields }) =
  space
    <> formatName settings indent blank name
    <> foldMap formatField fields
  where
  lines = singleOrMultiline constructor
  indented = indent <> tabSize
  { indentNext, prefix } = case lines of
    MultipleLines → { indentNext: indented, prefix: newline <> indented }
    SingleLine → { indentNext: indent, prefix: space }

  formatField typo = prefix <> formatType settings indentNext lines typo

-- | Formats the `data Maybe a` part of `data Maybe a = Just a | Nothing`
formatDataHead ∷ Settings → Indentation → CST.DataHead Void → String
formatDataHead settings indent { keyword, name, vars } =
  formatSourceToken settings indent blank keyword
    <> formatName settings indent space name
    <> formatTypeVarBindings settings indent vars

formatDataMembers ∷
  Settings →
  Indentation →
  CST.DataMembers →
  String
formatDataMembers settings@{ tabSize } indent dataMembers = case dataMembers of
  CST.DataAll sourceToken →
    formatSourceToken settings indent blank sourceToken
  CST.DataEnumerated delimited →
    prefix
      <> formatDelimited
          settings
          indent
          (formatName settings indentNext blank)
          delimited
    where
    { indentNext, prefix } = case lines of
      MultipleLines → { indentNext: indent <> tabSize, prefix: newline <> indent }
      SingleLine → { indentNext: indent, prefix: blank }
  where
  lines = singleOrMultiline dataMembers

formatDeclarations ∷
  Settings →
  Array (CST.Declaration Void) →
  String
formatDeclarations settings decls =
  foldMap
    formatWithNext
    (Array.zip decls (Array.snoc (Just <$> Array.drop 1 decls) Nothing))
  where
  formatWithNext (declaration /\ maybeFollowingDeclaration) =
    newline <> formatDeclaration settings blank followedByBlanks declaration
    where
    followedByBlanks = case maybeFollowingDeclaration of
      Nothing → true -- always true so we have a final newline
      Just following → isPrecededByBlankLines following

formatDeclaration ∷ Settings → Indentation → Boolean → CST.Declaration Void → String
formatDeclaration settings@{ tabSize } indent followedByBlankLines declaration = case declaration of
  CST.DeclData dataHead maybeConstructors → do
    formatDataHead settings indent dataHead
      <> foldMap formatDataConstructors maybeConstructors
      <> if followedByBlankLines then newline else blank
    where
    formatDataConstructors (equals /\ constructors) =
      (newline <> indented)
        <> formatSourceToken settings indented blank equals
        <> formatSeparated
            settings
            MultipleLines
            indented
            blank
            (formatDataConstructor settings indented)
            constructors

  CST.DeclType dataHead equals typo →
    formatDataHead settings indent dataHead
      <> newline
      <> indented
      <> formatSourceToken settings indented blank equals
      <> space
      <> formatType
          settings (indented <> tabSize) (singleOrMultiline typo) typo
      <> if followedByBlankLines then newline else blank

  CST.DeclNewtype dataHead equals name typo → do
    formatDataHead settings indent dataHead
      <> (newline <> indented)
      <> formatSourceToken settings indented blank equals
      <> space
      <> formatName settings indented blank name
      <> prefix
      <> formatType settings indented lines typo
      <> if followedByBlankLines then newline else blank
    where
    prefix = case singleOrMultilineBetween name typo of
      MultipleLines → newline <> indented
      SingleLine → space

  CST.DeclClass classHead members → do
    formatClassHead lines settings indent classHead
      <> foldMap formatMember members
      <> if followedByBlankLines then newline else blank
    where
    formatMember (whereClause /\ labeleds) =
      formatSourceToken settings indent space whereClause
        <> intercalateMap (newline <> indented) formatLabeledMember labeleds
    formatLabeledMember labeled =
      newline <> indented <> formatLabeledNameType settings indented labeled

  CST.DeclInstanceChain instances →
    formatSeparated
      settings
      (singleOrMultiline instances)
      indent
      space
      (formatInstance settings indent)
      instances
      <> if followedByBlankLines then newline else blank

  CST.DeclDerive sourceToken newtypeToken instanceHead →
    formatSourceToken settings indent blank sourceToken
      <> foldMap (formatSourceToken settings indent space) newtypeToken
      <> space
      <> formatInstanceHead settings indent instanceHead
      <> if followedByBlankLines then newline else blank

  CST.DeclKindSignature kindOfDeclaration labeled →
    formatSourceToken settings blank indent kindOfDeclaration
      <> space
      <> formatLabeledNameType settings indent labeled

  CST.DeclSignature labeled →
    formatLabeledNameType settings indent labeled

  CST.DeclValue valueBindingFields →
    formatValueBindingFields settings indent valueBindingFields
      <> if followedByBlankLines then newline else blank

  CST.DeclFixity fixityFields →
    formatFixityFields settings indent fixityFields <> newline

  CST.DeclForeign foreignToken importToken theForeign →
    formatSourceToken settings indent blank foreignToken
      <> formatSourceToken settings indent space importToken
      <> space
      <> formatForeign settings indent theForeign
      <> if followedByBlankLines then newline else blank

  CST.DeclRole typo roleToken name roles → do
    formatSourceToken settings tabSize indent typo
      <> space
      <> formatSourceToken settings tabSize indent roleToken
      <> space
      <> formatName settings indent blank name
      <> foldMap formatDeclRole roles
      <> if followedByBlankLines then newline else blank
    where
    formatDeclRole role =
      space <> formatRole settings indent role

  CST.DeclError e → absurd e

  where
  indented = indent <> tabSize
  lines = singleOrMultiline declaration

formatDelimited ∷
  ∀ a.
  Settings →
  Indentation →
  (a → String) →
  CST.Delimited a →
  String
formatDelimited settings indent formatValue delimited =
  formatWrapped
    settings
    indent
    (foldMap (formatSeparated settings lines indent space formatValue))
    delimited
  where
  lines = singleOrMultiline delimited

formatDelimitedNonEmpty ∷
  ∀ a.
  RangeOf a ⇒
  Settings →
  Lines →
  Indentation →
  (a → String) →
  CST.DelimitedNonEmpty a →
  String
formatDelimitedNonEmpty settings lines indent formatValue =
  formatWrapped settings indent
    $ formatSeparated settings lines indent space formatValue

formatDoBlock ∷
  Lines →
  Settings →
  Indentation →
  CST.DoBlock Void →
  String
formatDoBlock lines settings@{ tabSize } indent block =
  formatSourceToken settings indent blank block.keyword
    <> foldMap
        formatOne
        block.statements
  where
  { indentNext, prefix } = case lines of
    MultipleLines →
      { indentNext: indent <> tabSize
      , prefix: newline <> indent <> tabSize
      }
    SingleLine → { indentNext: indent, prefix: space }

  formatOne doStatement =
    prefix
      <> formatDoStatement settings indentNext doStatement

formatDoStatement ∷
  Settings →
  Indentation →
  CST.DoStatement Void →
  String
formatDoStatement settings@{ tabSize } indent doStatement = case doStatement of
  CST.DoBind binder arrow expr → do
    formatBinder settings indent binder
      <> formatSourceToken settings indent space arrow
      <> formatExprPrefix lines settings indent expr

  CST.DoDiscard expr →
    formatExpr settings indent expr

  CST.DoLet letKeyword letBindings →
    case lines of
      MultipleLines →
        formatSourceToken settings indent blank letKeyword
          <> (newline <> indented)
          <> (formatLetBindings settings indented) letBindings
      SingleLine →
        formatSourceToken settings indent blank letKeyword
          <> formatLetBinding settings space space blank (NE.head letBindings)
  CST.DoError e → absurd e
  where
  lines = singleOrMultiline doStatement
  indented = indent <> tabSize

formatExports ∷
  Settings →
  Maybe (CST.DelimitedNonEmpty (CST.Export Void)) →
  String
formatExports settings@{ tabSize } = case _ of
  Nothing → blank
  Just exports → do
    let
      indent = tabSize
      prefix = case lines of
        MultipleLines → newline <> indent
        SingleLine → space
    prefix
      <> formatDelimitedNonEmpty
          settings
          lines
          indent
          (formatExport settings indent)
          exports
    where
    lines = singleOrMultiline exports

formatExport ∷
  Settings →
  Indentation →
  CST.Export Void →
  String
formatExport settings@{ tabSize } indent export = case export of
  CST.ExportClass classToken name →
    formatSourceToken settings indent blank classToken
      <> formatName settings indent space name
  CST.ExportKind kind name →
    formatSourceToken settings indent blank kind
      <> formatName settings indent space name
  CST.ExportModule moduleToken name →
    formatSourceToken settings indent blank moduleToken
      <> formatName settings indent space name
  CST.ExportOp name →
    formatName settings indent blank name
  CST.ExportType name dataMembers →
    formatName settings indent blank name
      <> prefix
      <> foldMap (formatDataMembers settings indentNext) dataMembers
    where
    indented = indent <> tabSize

    { indentNext, prefix } = case lines of
      MultipleLines → { indentNext: indented, prefix: newline <> indented }
      SingleLine → { indentNext: indent, prefix: blank }
  CST.ExportTypeOp typo name →
    formatSourceToken settings indent blank typo
      <> formatName settings indent space name
  CST.ExportValue name →
    formatName settings indent blank name
  CST.ExportError e → absurd e
  where
  lines = singleOrMultiline export

formatExpr ∷
  Settings →
  Indentation →
  CST.Expr Void →
  String
formatExpr settings@{ tabSize } indent expr = case expr of
  CST.ExprAdo adoBlock →
    formatAdoBlock lines settings indent adoBlock

  CST.ExprApp expr1 expressions →
    formatExpr settings indent expr1
      <> foldMap formatApp
          (NE.zip (NE.cons expr1 expressions) expressions)
    where
    formatApp (prev /\ curr) = do
      let appLines = singleOrMultilineBetween prev curr
      formatExprPrefix appLines settings indent curr

  CST.ExprArray delimited →
    formatArray
      settings
      lines
      indent
      (formatExpr settings indented)
      delimited
    where
    indented = case lines of
      MultipleLines → indent <> tabSize
      SingleLine → indent

  CST.ExprBoolean boolean _ →
    formatSourceToken settings indent blank boolean

  CST.ExprCase caseOf →
    formatCaseOf lines settings indent caseOf

  CST.ExprChar char _ →
    formatSourceToken settings indent blank char

  CST.ExprConstructor name →
    formatQualifiedName settings indent blank name

  CST.ExprDo doBlock →
    formatDoBlock lines settings indent doBlock

  CST.ExprHole hole →
    formatName settings indent blank hole

  CST.ExprIdent name →
    formatQualifiedName settings indent blank name

  CST.ExprIf ifThenElse →
    formatIfThenElse lines settings indent ifThenElse

  CST.ExprInfix expr1 expressions →
    formatExpr settings indent expr1
      <> prefixFirst
      <> foldMap formatOne expressions
    where
    { indentNext, indented, prefixFirst, prefix } = case lines of
      MultipleLines →
        { indentNext: indent <> tabSize <> tabSize
        , indented: indent <> tabSize
        , prefixFirst: newline <> indent <> tabSize
        , prefix: newline <> indent <> tabSize <> tabSize
        }
      SingleLine →
        { indentNext: indent
        , indented: indent
        , prefixFirst: space
        , prefix: space
        }

    formatOne (wrapped /\ expression) =
      formatWrapped settings indentNext (formatExpr settings indented) wrapped
        <> prefix
        <> formatExpr settings indentNext expression

  CST.ExprLambda lambda →
    formatLambda lines settings indent lambda

  CST.ExprLet letIn →
    formatLetIn lines settings indent letIn

  CST.ExprNegate negative expression →
    formatSourceToken settings indent blank negative
      <> formatExpr settings indent expression

  CST.ExprNumber number _ →
    formatSourceToken settings indent blank number

  CST.ExprOp expr1 expressions →
    formatExpr settings indent expr1
      <> foldMap formatExpression expressions
    where
    { indentNext, indented, operatorPrefix } = case lines of
      MultipleLines →
        { indentNext: indent <> tabSize <> tabSize
        , indented: indent <> tabSize
        , operatorPrefix: newline <> indent <> tabSize
        }
      SingleLine →
        { indentNext: indent <> tabSize
        , indented: indent
        , operatorPrefix: space
        }

    formatExpression (operator /\ expression) = do
      let
        -- To preserve alignment of brackets, commas, etc.
        -- we need to add a newline if there's a multiline record,
        -- array, etc.
        expressionPrefix = case expression of
          CST.ExprRecord _ → case singleOrMultiline expression of
            MultipleLines →
              (newline <> indent <> tabSize <> tabSize)
            SingleLine → space

          CST.ExprArray _ → case singleOrMultiline expression of
            MultipleLines →
              (newline <> indent <> tabSize <> tabSize)
            SingleLine → space

          _ → space

        operatorSuffix = do
          if hasNonWhitespaceTrailingComment (unsafeFirstTokenOf operator) then
            newline <> indented <> space
          else
            blank
      operatorPrefix
        <> formatQualifiedName settings indented blank operator
        <> operatorSuffix
        <> expressionPrefix
        <> formatExpr settings indentNext expression

  CST.ExprOpName name → do
    formatQualifiedName settings indent blank name

  CST.ExprParens wrapped → do
    formatParens lines settings indent (formatExpr settings) wrapped

  CST.ExprRecord delimited → do
    formatRecord settings indent formatOne delimited
    where
    formatOne =
      formatRecordLabeled
        settings
        indent
        (formatExpr settings)

  CST.ExprRecordAccessor recordAccessor → do
    formatRecordAccessor lines settings indent recordAccessor

  CST.ExprRecordUpdate exprRecordUpdate delimitedNonEmpty → do
    formatExpr settings indent exprRecordUpdate
      <> prefix
      <> formatRecordNonEmpty
          settings
          indented
          (formatRecordUpdate settings indented)
          delimitedNonEmpty
    where
    { indented, prefix } = case lines of
      MultipleLines → { indented: indent <> tabSize, prefix: newline <> indent <> tabSize }
      SingleLine → { indented: indent, prefix: space }

  CST.ExprSection section → formatSourceToken settings indent blank section

  CST.ExprString string _ → formatSourceToken settings indent blank string

  CST.ExprTyped exprTyped colons typo → do
    formatExpr settings indent exprTyped
      <> formatSourceToken settings indent space colons
      <> prefix
      <> formatType settings indented lines typo
    where
    { indented, prefix } = case lines of
      MultipleLines → { indented: indent <> tabSize, prefix: newline <> indent <> tabSize }
      SingleLine → { indented: indent, prefix: space }

  CST.ExprInt int _ → formatSourceToken settings indent blank int

  CST.ExprError e → absurd e

  where
  lines = singleOrMultiline expr

formatExprPrefix ∷ Lines → Settings → Indentation → CST.Expr Void → String
formatExprPrefix lines settings@{ tabSize } indent expr =
  prefix <> formatExpr settings indented expr
  where
  indented = case expr of
    CST.ExprAdo _ → indent
    CST.ExprApp _ _ → indent <> tabSize
    CST.ExprArray _ → indent <> tabSize
    CST.ExprBoolean _ _ → indent <> tabSize
    -- Depends if you want to keep case on the same line
    CST.ExprCase c →
      if sourceTokenIsPrecededByNewline c.keyword then
        indent <> tabSize
      else
        indent
    CST.ExprChar _ _ → indent <> tabSize
    CST.ExprConstructor _ → indent <> tabSize
    CST.ExprDo _ → indent
    CST.ExprHole _ → indent <> tabSize
    CST.ExprIdent _ → indent <> tabSize
    CST.ExprIf _ → indent <> tabSize
    CST.ExprInfix _ _ → indent <> tabSize
    CST.ExprLambda _ → indent
    CST.ExprLet _ → indent <> tabSize
    CST.ExprNegate _ _ → indent <> tabSize
    CST.ExprNumber _ _ → indent <> tabSize
    CST.ExprOp _ _ → indent <> tabSize
    CST.ExprOpName _ → indent <> tabSize
    CST.ExprParens _ → indent <> tabSize
    CST.ExprRecord _ → indent <> tabSize
    CST.ExprRecordAccessor _ → indent <> tabSize
    CST.ExprRecordUpdate _ _ → indent <> tabSize
    CST.ExprSection _ → indent <> tabSize
    CST.ExprString _ _ → indent <> tabSize
    CST.ExprTyped _ _ _ → indent <> tabSize
    CST.ExprInt _ _ → indent <> tabSize
    CST.ExprError e → absurd e

  multiLine = case expr of
    CST.ExprAdo _ → space
    CST.ExprApp _ _ → newline <> indented
    CST.ExprArray _ → newline <> indented
    CST.ExprBoolean _ _ → newline <> indented
    -- Depends if you want to keep case on the same line
    CST.ExprCase c →
      if sourceTokenIsPrecededByNewline c.keyword then
        newline <> indented
      else
        space
    CST.ExprChar _ _ → newline <> indented
    CST.ExprConstructor _ → newline <> indented
    CST.ExprDo _ → space
    CST.ExprHole _ → newline <> indented
    CST.ExprIdent _ → newline <> indented
    CST.ExprIf _ → newline <> indented
    CST.ExprInfix _ _ → newline <> indented
    CST.ExprLambda _ → space
    CST.ExprLet _ → newline <> indented
    CST.ExprNegate _ _ → newline <> indented
    CST.ExprNumber _ _ → newline <> indented
    CST.ExprOp _ _ → newline <> indented
    CST.ExprOpName _ → newline <> indented
    CST.ExprParens _ → newline <> indented
    CST.ExprRecord _ → newline <> indented
    CST.ExprRecordAccessor _ → newline <> indented
    CST.ExprRecordUpdate _ _ → newline <> indented
    CST.ExprSection _ → newline <> indented
    CST.ExprString _ _ → newline <> indented
    CST.ExprTyped _ _ _ → newline <> indented
    CST.ExprInt _ _ → newline <> indented
    CST.ExprError e → absurd e

  prefix ∷ String
  prefix = case lines of
    MultipleLines → multiLine
    SingleLine → space

formatExprPrefixElseIf ∷
  Lines →
  Settings →
  Indentation →
  CST.Expr Void →
  String
formatExprPrefixElseIf lines settings indent expr = case expr of
  CST.ExprAdo _ → formatExprPrefix lines settings indent expr
  CST.ExprApp _ _ → formatExprPrefix lines settings indent expr
  CST.ExprArray _ → formatExprPrefix lines settings indent expr
  CST.ExprBoolean _ _ → formatExprPrefix lines settings indent expr
  CST.ExprCase _ → formatExprPrefix lines settings indent expr
  CST.ExprChar _ _ → formatExprPrefix lines settings indent expr
  CST.ExprConstructor _ → formatExprPrefix lines settings indent expr
  CST.ExprDo _ → formatExprPrefix lines settings indent expr
  CST.ExprHole _ → formatExprPrefix lines settings indent expr
  CST.ExprIdent _ → formatExprPrefix lines settings indent expr
  CST.ExprIf _ → space <> formatExpr settings indent expr
  CST.ExprInfix _ _ → formatExprPrefix lines settings indent expr
  CST.ExprLambda _ → formatExprPrefix lines settings indent expr
  CST.ExprLet _ → formatExprPrefix lines settings indent expr
  CST.ExprNegate _ _ → formatExprPrefix lines settings indent expr
  CST.ExprNumber _ _ → formatExprPrefix lines settings indent expr
  CST.ExprOp _ _ → formatExprPrefix lines settings indent expr
  CST.ExprOpName _ → formatExprPrefix lines settings indent expr
  CST.ExprParens _ → formatExprPrefix lines settings indent expr
  CST.ExprRecord _ → formatExprPrefix lines settings indent expr
  CST.ExprRecordAccessor _ → formatExprPrefix lines settings indent expr
  CST.ExprRecordUpdate _ _ → formatExprPrefix lines settings indent expr
  CST.ExprSection _ → formatExprPrefix lines settings indent expr
  CST.ExprString _ _ → formatExprPrefix lines settings indent expr
  CST.ExprTyped _ _ _ → formatExprPrefix lines settings indent expr
  CST.ExprInt _ _ → formatExprPrefix lines settings indent expr
  CST.ExprError e → absurd e

formatFixityFields ∷
  Settings →
  Indentation →
  CST.FixityFields →
  String
formatFixityFields settings indent { keyword: infixToken /\ _, prec: precedence /\ _, operator } =
  formatSourceToken settings indent blank infixToken
    <> formatSourceToken settings indent space precedence
    <> space
    <> formatFixityOp settings indent operator

formatFixityOp ∷
  Settings →
  Indentation →
  CST.FixityOp →
  String
formatFixityOp settings indent = case _ of
  CST.FixityType typo name as op →
    formatSourceToken settings indent blank typo
      <> formatQualifiedName settings indent space name
      <> formatSourceToken settings indent space as
      <> formatName settings indent space op
  CST.FixityValue name as op →
    formatQualifiedName settings indent blank name
      <> formatSourceToken settings indent space as
      <> formatName settings indent space op

formatForeign ∷
  Settings →
  Indentation →
  CST.Foreign Void →
  String
formatForeign settings indent = case _ of
  CST.ForeignData dataToken labeled →
    formatSourceToken settings indent blank dataToken
      <> space
      <> formatLabeledNameKind settings indent labeled
  CST.ForeignKind kind name →
    formatSourceToken settings indent blank kind
      <> formatName settings indent space name
  CST.ForeignValue labeled →
    formatLabeledNameType settings indent labeled

formatGuarded ∷
  Settings →
  Lines →
  Indentation →
  CST.Guarded Void →
  String
formatGuarded settings@{ tabSize } lines indent = case _ of
  CST.Guarded guardedExprs →
    foldMap formatOne guardedExprs
  CST.Unconditional separator whereToken →
    formatSourceToken settings indent space separator
      <> formatWhere settings indent whereToken
  where
  indented = indent <> tabSize
  formatOne guardedExpr = do
    let
      prefix = case lines of
        MultipleLines → newline <> indented
        SingleLine → space
    prefix <> formatGuardedExpr settings indented guardedExpr

formatGuardedExpr ∷
  Settings →
  Indentation →
  CST.GuardedExpr Void →
  String
formatGuardedExpr settings@{ tabSize } indent (CST.GuardedExpr expr) =
  formatSourceToken settings indent blank expr.bar
    <> space
    <> formatSeparated
        settings
        (singleOrMultilineFromRange (rangeOfSeparated rangeOfPatternGuard expr.patterns))
        indent
        space
        (formatPatternGuard settings indented)
        expr.patterns
    <> space
    <> formatSourceToken settings indent blank expr.separator
    <> formatWhere settings indent expr.where
  where
  indented = indent <> tabSize

formatIfThenElse ∷
  Lines →
  Settings →
  Indentation →
  CST.IfThenElse Void →
  String
formatIfThenElse lines settings indent block =
  formatSourceToken settings indent blank block.keyword
    <> space
    <> formatExpr settings indent block.cond
    <> space
    <> formatSourceToken settings indent blank block."then"
    <> formatExprPrefix lines settings indent block."true"
    <> prefix
    <> formatSourceToken settings indent blank block."else"
    <> formatExprPrefixElseIf lines settings indent block."false"
  where
  prefix = case lines of
    MultipleLines → newline <> indent
    SingleLine → space

formatImports ∷
  Settings →
  Array (CST.ImportDecl Void) →
  String
formatImports settings = case _ of
  [] → blank
  imports →
    foldMap formatOne imports
      <> newline
  where
  formatOne importDecl =
    newline
      <> formatImportDeclaration settings blank importDecl

formatImport ∷
  Settings →
  Indentation →
  CST.Import Void →
  String
formatImport settings@{ tabSize } indent theImport = case theImport of
  CST.ImportClass classToken name →
    formatSourceToken settings indent blank classToken
      <> formatName settings indent space name
  CST.ImportKind kind name →
    formatSourceToken settings indent blank kind
      <> formatName settings indent space name
  CST.ImportOp name →
    formatName settings indent blank name
  CST.ImportType name dataMembers →
    formatName settings indent blank name
      <> foldMap (formatDataMembers settings indentNext) dataMembers
    where
    indentNext = case lines of
      MultipleLines → indent <> tabSize
      SingleLine → indent
  CST.ImportTypeOp typo name →
    formatSourceToken settings indent blank typo
      <> formatName settings indent space name
  CST.ImportValue name →
    formatName settings indent blank name
  CST.ImportError e → absurd e
  where
  lines = singleOrMultiline theImport

formatImportDeclaration ∷
  Settings →
  Indentation →
  CST.ImportDecl Void →
  String
formatImportDeclaration settings@{ tabSize } indent importDecl =
  formatSourceToken settings indent blank importToken
    <> formatName settings indent space name
    <> foldMap formatOne imports
    <> foldMap formatQualified qualified
  where
  CST.ImportDecl { keyword: importToken, module: name, names: imports, qualified } = importDecl

  lines = singleOrMultiline importDecl

  indented = indent <> tabSize

  formatOne = case _ of
    Just hiding /\ imports → do
      let
        indentNext = indented <> tabSize
        hidingPrefix = case lines of
          MultipleLines → newline <> indented
          SingleLine → space
        importPrefix = case lines of
          MultipleLines → newline <> indentNext
          SingleLine → space
      hidingPrefix
        <> formatSourceToken settings indented blank hiding
        <> importPrefix
        <> formatDelimitedNonEmpty
            settings
            lines
            indentNext
            (formatImport settings indented)
            imports
    Nothing /\ imports → do
      let
        importPrefix = case lines of
          MultipleLines → newline <> indented
          SingleLine → space
      importPrefix
        <> formatDelimitedNonEmpty
            settings
            lines
            indented
            (formatImport settings indented)
            imports

  formatQualified (as /\ name) = do
    let
      prefix = case lines of
        MultipleLines → newline <> indented
        SingleLine → space
    prefix
      <> formatSourceToken settings indented blank as
      <> formatName settings indented space name

formatInstance ∷
  Settings →
  Indentation →
  CST.Instance Void →
  String
formatInstance settings@{ tabSize } indent (CST.Instance { head, body }) =
  formatInstanceHead settings indent head
    <> foldMap formatBindings body
  where
  formatBindings (whereClause /\ instanceBindings) =
    formatSourceToken settings indent space whereClause
      <> foldMap formatBinding instanceBindings
  formatBinding instanceBinding =
    newline
      <> indented
      <> formatInstanceBinding settings indented instanceBinding
  indented = indent <> tabSize

formatInstanceHead ∷
  Settings →
  Indentation →
  CST.InstanceHead Void →
  String
formatInstanceHead settings@{ tabSize } indent head =
  formatSourceToken settings indent blank head.keyword
    <> foldMap formatNameAndSeparator head.nameAndSeparator
    <> foldMap formatInstanceConstraint head.constraints
    <> formatQualifiedName settings indented prefix head.className
    <> foldMap formatInstanceType head.types
  where
  lines = rangeOfInstanceHead head
  { indentNext, indented, prefix } = case lines of
    MultipleLines →
      { indentNext: indent <> tabSize <> tabSize
      , indented: indent <> tabSize
      , prefix: newline <> indent <> tabSize
      }
    SingleLine →
      { indentNext: indent
      , indented: indent
      , prefix: space
      }
  typePrefix = case Array.last head.types of
    Just lastType → case singleOrMultilineBetween head.className lastType of
      MultipleLines → newline <> indentNext
      SingleLine → space
    Nothing → space

  formatNameAndSeparator { name: name, separator: colons } =
    formatName settings indent space name
      <> formatSourceToken settings indent space colons

  formatInstanceConstraint (constraints /\ arrow) =
    prefix
      <> formatOneOrDelimited
          settings
          lines
          indented
          (\typo → formatType settings indentNext (singleOrMultiline typo) typo)
          constraints
      <> formatSourceToken settings indented space arrow

  formatInstanceType typo =
    typePrefix
      <> formatType settings indentNext (singleOrMultiline typo) typo

formatInstanceBinding ∷
  Settings →
  Indentation →
  CST.InstanceBinding Void →
  String
formatInstanceBinding settings indent = case _ of
  CST.InstanceBindingSignature labeled →
    formatLabeledNameType settings indent labeled
  CST.InstanceBindingName valueBindingFields →
    formatValueBindingFields settings indent valueBindingFields

formatLabeled ∷
  ∀ a b.
  RangeOf a ⇒
  RangeOf b ⇒
  Settings →
  Indentation →
  (a → String) →
  (Indentation → b → String) →
  CST.Labeled a b →
  String
formatLabeled settings@{ tabSize } indent formatLabel formatValue
  labeled@(CST.Labeled { label, separator, value }) =
  formatLabel label
    <> formatSourceToken settings indented space separator
    <> prefix
    <> formatValue indented value
  where
  { indented, prefix } = case singleOrMultiline labeled of
    MultipleLines → do
      { indented: indent <> tabSize
      , prefix: newline <> indent <> tabSize
      }
    SingleLine → { indented: indent, prefix: space }

formatLabeledName ∷
  ∀ n.
  RangeOf (CST.Name n) ⇒
  Settings →
  Indentation →
  CST.Labeled (CST.Name n) (CST.Type Void) →
  String
formatLabeledName settings indent labeledName =
  formatLabeled
    settings
    indent
    (formatName settings indent blank)
    (\name → formatType settings name (singleOrMultiline labeledName))
    labeledName

formatLabeledNameKind ∷
  ∀ a.
  Settings →
  Indentation →
  CST.Labeled
    (CST.Name a)
    (CST.Type Void) →
  String
formatLabeledNameKind settings indent labeled =
  formatLabeled
    settings
    indent
    (formatName settings indent blank)
    (\typo → formatType settings typo (singleOrMultiline labeled))
    labeled

formatLabeledNameType ∷
  ∀ a.
  Settings →
  Indentation →
  CST.Labeled (CST.Name a) (CST.Type Void) →
  String
formatLabeledNameType settings indent labeled =
  formatLabeled
    settings
    indent
    (formatName settings indent blank)
    formatOne
    labeled
  where
  formatOne typo = formatType settings typo (singleOrMultiline labeled)

formatLambda ∷
  Lines →
  Settings →
  Indentation →
  CST.Lambda Void →
  String
formatLambda lines settings indent { symbol, binders, arrow, body } =
  formatSourceToken settings indent blank symbol
    <> foldMap formatOne binders
    <> formatSourceToken settings indent blank arrow
    <> formatExprPrefix lines settings indent body
  where
  formatOne binder =
    formatBinder settings indent binder <> space

formatLetBindings ∷ Settings → Indentation → NonEmptyArray (CST.LetBinding Void) → String
formatLetBindings settings indent letBindings =
  foldMapWithIndex
    formatLetBindingWithFollowing
    ( NE.zip letBindings
        (NE.snoc' (Just <$> NE.tail letBindings) Nothing)
    )
  where
  formatLetBindingWithFollowing idx (current /\ maybeFollowing) = do
    formatLetBinding settings indent prefix suffix current
    where
    prefix = if idx == 0 then blank else (newline <> indent)
    suffix = case maybeFollowing of
      Nothing → blank -- no newline on last entry
      Just followingLetBinding →
        if isPrecededByBlankLines followingLetBinding then
          newline
        else
          blank

formatLetBinding ∷
  Settings →
  Indentation →
  Prefix →
  Suffix →
  CST.LetBinding Void →
  String
formatLetBinding settings indent prefix suffix = case _ of
  CST.LetBindingName valueBindingFields → do
    prefix
      <> formatValueBindingFields settings indent valueBindingFields
      <> suffix
  CST.LetBindingPattern binder equals whereBindings → do
    prefix
      <> formatBinder settings indent binder
      <> formatSourceToken settings indent space equals
      <> formatWhere settings indent whereBindings
      <> suffix
  CST.LetBindingSignature labeled → do
    prefix
      <> formatLabeledNameType settings indent labeled
  CST.LetBindingError e → absurd e

formatLetIn ∷
  Lines →
  Settings →
  Indentation →
  CST.LetIn Void →
  String
formatLetIn lines settings@{ tabSize } indent letIn =
  formatSourceToken settings indent blank letIn.keyword
    <> prefix
    <> (formatLetBindings settings indentNext letIn.bindings)
    <> inPrefix
    <> formatSourceToken settings indent blank letIn.in
    <> prefix
    <> formatExpr settings indentNext letIn.body
  where
  { inPrefix, indentNext, prefix } = case lines of
    MultipleLines →
      { inPrefix: newline <> indent
      , indentNext: indent <> tabSize
      , prefix: newline <> indent <> tabSize
      }
    SingleLine → { inPrefix: space, indentNext: indent, prefix: space }

formatModule ∷ Settings → CST.Module Void → String
formatModule settings (CST.Module { header, body }) =
  formatModuleHeader settings header
    <> formatModuleBody settings body

formatModuleHeader ∷ Settings → CST.ModuleHeader Void → String
formatModuleHeader settings (CST.ModuleHeader header) =
  formatSourceToken settings blank blank header.keyword
    <> formatName settings blank space header.name
    <> formatExports settings header.exports
    <> formatSourceToken settings blank space header.where
    <> newline
    <> formatImports settings header.imports

formatModuleBody ∷ Settings → CST.ModuleBody Void → String
formatModuleBody settings (CST.ModuleBody { decls, trailingComments }) =
  formatDeclarations settings decls
    <> formatCommentsTrailingModule trailingComments

formatName ∷ ∀ a. Settings → Indentation → Prefix → CST.Name a → String
formatName settings indent prefix (CST.Name { token }) =
  formatSourceToken settings indent prefix token

formatOneOrDelimited ∷
  ∀ a.
  RangeOf a ⇒
  Settings →
  Lines → Indentation → (a → String) → CST.OneOrDelimited a → String
formatOneOrDelimited settings lines indent formatValue = case _ of
  CST.One a → formatValue a
  CST.Many delimitedNonEmpty →
    formatDelimitedNonEmpty settings lines indent formatValue delimitedNonEmpty

formatParens ∷
  ∀ a.
  Lines →
  Settings →
  Indentation →
  (Indentation → a → String) →
  CST.Wrapped a →
  String
formatParens lines settings@{ tabSize } indent formatValue wrapped = do
  formatWrapped settings indent (formatValue indented) wrapped
  where
  indented = case lines of
    MultipleLines → indent <> tabSize
    SingleLine → indent

formatPatternGuard ∷
  Settings →
  Indentation →
  CST.PatternGuard Void →
  String
formatPatternGuard settings indent patternGuard@(CST.PatternGuard { binder, expr }) = case binder of
  Just (theBinder /\ arrow) →
    formatBinder settings indent theBinder
      <> formatSourceToken settings indent space arrow
      <> formatExprPrefix lines settings indent expr
  Nothing → formatExpr settings indent expr
  where
  lines = singleOrMultilineFromRange (rangeOfPatternGuard patternGuard)

formatQualifiedName ∷
  ∀ a.
  Settings →
  Indentation →
  Prefix →
  CST.QualifiedName a →
  String
formatQualifiedName settings indent prefix (CST.QualifiedName { token }) =
  formatSourceToken settings indent prefix token

formatRecord ∷
  ∀ a.
  Settings →
  Indentation →
  (a → String) →
  CST.Delimited a →
  String
formatRecord settings indent formatValue = case _ of
  CST.Wrapped { open, value: Nothing, close } → do
    formatSourceToken settings indent blank open
      <> formatSourceToken settings indent preservedNewline close
    where
    preservedNewline =
      if sourceTokenIsPrecededByNewline close then
        newline <> indent
      else
        blank
  CST.Wrapped { open, value: Just record, close } →
    formatRecordNonEmpty
      settings
      indent
      formatValue
      (CST.Wrapped { open, value: record, close })

formatRecordNonEmpty ∷
  ∀ a.
  Settings →
  Indentation →
  (a → String) →
  CST.DelimitedNonEmpty a →
  String
formatRecordNonEmpty settings indent formatValue record =
  formatWrapped settings indent formatOne record
  where
  lines = singleOrMultiline record
  indentation = case lines of
    MultipleLines → blank
    SingleLine → space
  formatOne separated =
    indentation
      <> formatSeparated settings lines indent space formatValue separated
      <> indentation

formatRecordAccessor ∷
  Lines →
  Settings →
  Indentation →
  CST.RecordAccessor Void →
  String
formatRecordAccessor lines settings@{ tabSize } indent { expr, dot, path } =
  formatExpr settings indentNext expr
    <> prefix
    <> formatSourceToken settings indentNext blank dot
    <> formatSeparated
        settings
        lines
        indentNext
        blank
        (formatName settings indent blank)
        path
  where
  indented = indent <> tabSize
  { indentNext, prefix } = case lines of
    MultipleLines → { indentNext: indented, prefix: newline <> indented }
    SingleLine → { indentNext: indent, prefix: blank }

formatRecordLabeled ∷
  ∀ a.
  RangeOf a ⇒
  Settings →
  Indentation →
  (Indentation → a → String) →
  CST.RecordLabeled a →
  String
formatRecordLabeled settings@{ tabSize } indent formatNext = case _ of
  CST.RecordPun name →
    formatName settings indent blank name
  CST.RecordField label colon field → do
    formatName settings indent blank label
      <> formatSourceToken settings indent blank colon
      <> prefix
      <> formatNext indentNext field
    where
    { indentNext, prefix } = case singleOrMultilineBetween label field of
      MultipleLines →
        { indentNext: indent <> tabSize <> tabSize
        , prefix: newline <> indent <> tabSize <> tabSize
        }
      SingleLine → { indentNext: indent, prefix: space }

formatRecordUpdate ∷
  Settings →
  Indentation →
  CST.RecordUpdate Void →
  String
formatRecordUpdate settings@{ tabSize } indent = case _ of
  CST.RecordUpdateBranch label delimitedNonEmpty → do
    let
      lines = singleOrMultiline delimitedNonEmpty
      { indentNext, prefix } = getIndent lines
    formatName settings indent blank label
      <> prefix
      <> formatRecordNonEmpty
          settings
          indentNext
          (formatRecordUpdate settings indentNext)
          delimitedNonEmpty
  CST.RecordUpdateLeaf label equals expr → do
    let
      lines = singleOrMultiline expr
      { indentNext, prefix } = getIndent lines
    formatName settings indent blank label
      <> formatSourceToken settings indent space equals
      <> prefix
      <> formatExpr settings indentNext expr
  where
  indented = indent <> tabSize
  getIndent = case _ of
    MultipleLines → { indentNext: indented, prefix: newline <> indented }
    SingleLine → { indentNext: indent, prefix: space }

formatRole ∷
  Settings →
  Indentation →
  CST.SourceToken
  /\
  CST.Role →
  String
formatRole settings@{ tabSize } indent (sourceToken /\ _) =
  formatSourceToken settings tabSize indent sourceToken

formatRow ∷
  Lines →
  Settings →
  Indentation →
  CST.Row Void →
  String
formatRow lines settings@{ tabSize } indent
  (CST.Row { labels, tail }) = case labels, tail of
  Nothing, Nothing → blank
  Just ls, Nothing →
    before
      <> formatSeparated settings lines indent space f ls
      <> after
    where
    { before, indented, after } = case lines of
      MultipleLines →
        { before: blank, indented: indent <> tabSize, after: blank }
      SingleLine → { before: space, indented: indent, after: space }

    f = formatLabeledName settings indented
  Nothing, Just (bar /\ tailType) → do
    before
      <> formatSourceToken settings indent blank bar
      <> space
      <> formatType settings indent lines tailType
      <> after
    where
    { before, after } = case lines of
      MultipleLines → { before: newline <> indent, after: blank }
      SingleLine → { before: space, after: space }
  Just ls, Just (bar /\ tailType) → do
    before
      <> formatSeparated settings lines indent space f ls
      <> prefix
      <> formatSourceToken settings indent blank bar
      <> space
      <> formatType settings indent lines tailType
      <> after
    where
    { before, indented, after, prefix } = case lines of
      MultipleLines →
        { before: blank
        , indented: indent <> tabSize
        , after: blank
        , prefix: newline <> indent
        }
      SingleLine →
        { before: space
        , indented: indent
        , after: space
        , prefix: space
        }

    f = formatLabeledName settings indented

formatSeparated ∷
  ∀ a.
  Settings →
  Lines →
  Indentation →
  Prefix →
  (a → String) →
  CST.Separated a →
  String
formatSeparated settings lines indent prefix formatValue
  (CST.Separated { head, tail }) = do
  formatValue head <> foldMap go tail
  where
  nextPrefix = case lines of
    MultipleLines → newline <> indent
    SingleLine → blank

  go ∷ CST.SourceToken /\ a → String
  go (separator /\ value) =
    nextPrefix
      <> formatSourceToken settings indent blank separator
      <> ensureNewlineAfterSeparator
      <> prefix
      <> formatValue value
    where
    ensureNewlineAfterSeparator =
      if hasNonWhitespaceTrailingComment separator then
        newline <> indent <> prefix
      else
        blank

formatSourceToken ∷ Settings → Indentation → Prefix → CST.SourceToken → String
formatSourceToken settings indent prefix
  { leadingComments, trailingComments, value } =
  formatCommentsLeading indent prefix leadingComments
    <> prefix
    <> print value
    <> formatCommentsTrailing prefix protectedTrailingComments
  where
  protectedTrailingComments = wrapTrailingComment <$> trailingComments

  print = case settings.sourceStyle of
    Nothing → Print.printToken
    Just style → printWithStyle style

  printWithStyle style v = case v of
    CST.TokLeftArrow _ → case style of
      CST.ASCII → "<-"
      CST.Unicode → "←"
    CST.TokRightArrow _ → case style of
      CST.ASCII → "->"
      CST.Unicode → "→"
    CST.TokRightFatArrow _ → case style of
      CST.ASCII → "=>"
      CST.Unicode → "⇒"
    CST.TokDoubleColon _ → case style of
      CST.ASCII → "::"
      CST.Unicode → "∷"
    CST.TokForall _ → case style of
      CST.ASCII → "forall"
      CST.Unicode → "∀"
    CST.TokSymbolArrow _ → case style of
      CST.ASCII → "(->)"
      CST.Unicode → "(→)"
    _ → Print.printToken v

formatType ∷
  Settings →
  Indentation →
  Lines →
  CST.Type Void →
  String
formatType settings@{ tabSize } indent lines t = case t of
  CST.TypeApp typo types →
    formatType settings indent (singleOrMultilineBetween typo (NE.head types)) typo
      <> foldMap formatTypeWithPredecessor
          (Array.zip typesArray (typo Array.: typesArray))
    where
    typesArray = NE.toArray types

    formatTypeWithPredecessor (curr /\ pred) =
      prefix
        <> formatType settings indented (singleOrMultiline curr) curr
      where
      { indented, prefix } = case singleOrMultilineBetween curr pred of
        MultipleLines →
          { indented: indent <> tabSize
          , prefix: newline <> indent <> tabSize
          }
        SingleLine →
          { indented: indent
          , prefix: space
          }

  CST.TypeArrow t1 arrow t2 →
    formatType settings indent typeArrowLines t1
      <> space
      <> formatSourceToken settings indent blank arrow
      <> prefix
      <> formatType settings indent (singleOrMultiline t2) t2
    where
    typeArrowLines = singleOrMultilineBetween t1 t2
    prefix = case typeArrowLines of
      MultipleLines → newline <> indent
      SingleLine → space

  CST.TypeArrowName arrowName → formatSourceToken settings indent blank arrowName

  CST.TypeVar name → formatName settings indent blank name

  CST.TypeConstructor qualifiedName → formatQualifiedName settings indent blank qualifiedName

  CST.TypeWildcard wildcard → formatSourceToken settings indent blank wildcard

  CST.TypeHole name → formatName settings indent blank name

  CST.TypeString typeString _ → formatSourceToken settings indent blank typeString

  CST.TypeRow wrappedRow → formatWrappedRow settings indent wrappedRow

  CST.TypeRecord wrappedRow → formatWrappedRow settings indent wrappedRow

  CST.TypeForall forAll typeVarBindings dot tailType →
    formatSourceToken settings indent blank forAll
      <> formatTypeVarBindings settings indent (NE.toArray typeVarBindings)
      <> formatSourceToken settings indent blank dot
      <> prefix
      <> formatType settings indent typeForAllLines tailType
    where
    typeForAllLines = singleOrMultilineBetween t tailType
    prefix = case typeForAllLines of
      MultipleLines → newline <> indent
      SingleLine → space

  CST.TypeKinded typo colons kind →
    formatType settings indent lines typo
      <> space
      <> formatSourceToken settings indent blank colons
      <> prefix
      <> formatType settings indented lines kind
    where
    { indented, prefix } = case lines of
      MultipleLines →
        { indented: indent <> tabSize, prefix: newline <> indent }
      SingleLine → { indented: indent, prefix: space }

  CST.TypeOp typo types →
    formatType settings indent (singleOrMultiline typo) typo
      <> foldMap formatOp types
    where
    formatOp (op /\ anotherType) =
      formatQualifiedName settings indented prefix op
        <> prefix
        <> formatType settings indented (singleOrMultiline anotherType)
            anotherType
      where
      { indented, prefix } = case lines of
        MultipleLines →
          { indented: indent <> tabSize, prefix: newline <> indent }
        SingleLine → { indented: indent, prefix: space }

  CST.TypeOpName op → formatQualifiedName settings indent blank op

  CST.TypeConstrained constraint arrow typo →
    formatType settings indent lines constraint
      <> space
      <> formatSourceToken settings indent blank arrow
      <> prefix
      <> formatType settings indent lines typo
    where
    prefix = case lines of
      MultipleLines → newline <> indent
      SingleLine → space

  CST.TypeParens wrapped →
    formatParens lines settings indent formatOne wrapped
    where
    formatOne typo wrappedType = formatType settings typo (singleOrMultiline wrappedType) wrappedType

  CST.TypeUnaryRow sourceToken typo →
    formatSourceToken settings indent blank sourceToken
      <> prefix
      <> formatType settings indent lines typo
    where
    prefix = case lines of
      MultipleLines → newline <> indent
      SingleLine → space

  CST.TypeError e → absurd e

formatTypeVarBindings ∷
  Settings →
  Indentation →
  Array (CST.TypeVarBinding Void) →
  String
formatTypeVarBindings settings indent =
  foldMap (pure space <> formatTypeVarBinding settings indent)

formatTypeVarBinding ∷
  Settings →
  Indentation →
  CST.TypeVarBinding Void →
  String
formatTypeVarBinding settings indent = case _ of
  CST.TypeVarName name → formatName settings indent blank name
  CST.TypeVarKinded wrapped → formatWrapped settings indent (formatLabeledName settings indent) wrapped

-- | This is for basics like function or value definitions
-- | ```
-- | x = true
-- | x <- [1,2,3]
-- | x | x `mod` 2 == 0 -> [1,2,3]
-- | ```
formatValueBindingFields ∷
  Settings →
  Indentation →
  CST.ValueBindingFields Void →
  String
formatValueBindingFields
  settings@{ tabSize } indent { name, binders, guarded } =
  formatName settings indent blank name
    <> foldMap formatValueBinder binders
    <> formatGuarded settings lines indent guarded
  where
  lines =
    singleOrMultilineBetweenSourceRanges
      (Array.last binders <#> rangeOf # fromMaybe (rangeOf name))
      (rangeOf guarded)

  formatValueBinder binder =
    valueBinderSeparator <> formatBinder settings innerIndent binder
    where
    valueBinderSeparator = case innerLines of
      SingleLine → space
      MultipleLines → newline <> indent <> tabSize
    innerIndent = case innerLines of
      SingleLine → indent
      MultipleLines → indent <> tabSize
    innerLines = case singleOrMultiline binder of
      SingleLine →
        if isPrecededByNewline binder then
          MultipleLines
        else
          SingleLine
      MultipleLines → MultipleLines

-- | This is also code blocks that *can* have a where
formatWhere ∷
  Settings →
  Indentation →
  CST.Where Void →
  String
formatWhere settings@{ tabSize } indent (CST.Where { expr, bindings }) =
  formatExprPrefix exprLines settings indent expr
    <> foldMap formatWhereAndBindings bindings
  where
  exprLines = case expr of
    CST.ExprChar _ _ → singleOrMultiline expr
    _ →
      if isPrecededByNewline expr then
        MultipleLines
      else
        singleOrMultiline expr
  indented = indent <> tabSize
  formatWhereAndBindings (whereToken /\ letBindings) =
    (wherePrefix <> newline <> indented)
      <> formatSourceToken settings indented blank whereToken
      <> whereSuffix
      <> (newline <> indented)
      <> (formatLetBindings settings indented) letBindings
    where
    wherePrefix =
      if precedingEmptyLines whereToken.leadingComments > 1 then newline else blank
    whereSuffix =
      if isPrecededByBlankLines (NE.head letBindings) then
        newline
      else
        blank

formatWrapped ∷
  ∀ a.
  Settings →
  Indentation →
  (a → String) →
  CST.Wrapped a →
  String
formatWrapped settings indent formatValue
  wrapped@(CST.Wrapped { open, value, close }) =
  formatSourceToken settings indent blank open
    <> ensureNewlineAfterOpen
    <> before
    <> formatValue value
    <> after
    <> formatSourceToken settings indent blank close
  where
  { before, after } = case singleOrMultiline wrapped of
    MultipleLines → { before: space, after: newline <> indent }
    SingleLine → { before: blank, after: blank }
  ensureNewlineAfterOpen =
    if hasNonWhitespaceTrailingComment open then
      newline <> indent <> before
    else
      blank

formatWrappedRow ∷
  Settings →
  Indentation →
  CST.Wrapped (CST.Row Void) →
  String
formatWrappedRow settings indent
  wrapped@(CST.Wrapped { open, value: row, close }) =
  formatSourceToken settings indent blank open
    <> before
    <> formatRow lines settings indent row
    <> after
    <> formatSourceToken settings indent blank close
  where
  lines = singleOrMultiline wrapped
  { before, after } = case row, lines of
    CST.Row { labels: Just _ }, MultipleLines → { before: space, after: newline <> indent }
    CST.Row { labels: Nothing }, MultipleLines → { before: blank, after: newline <> indent }
    _, SingleLine → { before: blank, after: blank }
