module Format (format) where

import Prelude

import Control.Lazy (defer)
import Data.Array (head)
import Data.Array.NonEmpty as NE
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (un)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import PureScript.CST.Print as Print
import PureScript.CST.Range (class RangeOf, class TokensOf, rangeOf, tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types as CST
import Unsafe.Coerce (unsafeCoerce)

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
  CST.DeclNewtype dataHead equals name typo -> do
    formatDataHead indentation indent dataHead
      <> newline 
      <> indented
      <> formatSourceToken indented blank equals
      <> space
      <> formatName indented blank name
      <> prefix
      <> formatType indentation indented typo
      <> newline
    where 
      prefix = case lines of
        MultiLine -> newline <> indented 
        SingleLine -> space
  CST.DeclClass classHead members -> do
    formatClassHead lines indentation indent classHead
      <> foldMap formatMember members
      <> newline   
    where 
      formatMember (whereClause /\ labeleds) =
        formatSourceToken indent space whereClause
          <> foldMap formatLabeledMember labeleds
      formatLabeledMember labeled = 
        newline <> indented <> formatLabeledNameType indentation indented labeled
  CST.DeclInstanceChain instances ->
    formatSeparated
      (singleOrMultiline instances)
      indented
      space
      (formatInstance indentation indented)
      instances
      <> newline
  CST.DeclDerive sourceToken newtype' instanceHead ->
    formatSourceToken indent blank sourceToken
      <> foldMap (formatSourceToken indent space) newtype'
      <> space
      <> formatInstanceHead indentation indent instanceHead
      <> newline
  CST.DeclKindSignature kindOfDeclaration labeled ->
    formatSourceToken indentation indent kindOfDeclaration
      <> space
      <> formatLabeledNameType indentation indent labeled
  CST.DeclSignature labeled ->
    formatLabeledNameType indentation indent labeled
  CST.DeclValue valueBindingFields ->
    formatValueBindingFields indentation indent valueBindingFields
  CST.DeclFixity fixityFields ->
    formatFixityFields indent fixityFields
  CST.DeclForeign foreign'' import'' foreign''' ->
    formatSourceToken indent blank foreign''
      <> formatSourceToken indent space import''
      <> space
      <> formatForeign lines indentation indent foreign'''
      <> newline
  CST.DeclRole type'' role'' name' roles -> do
    formatSourceToken indentation indent type''
      <> space
      <> formatSourceToken indentation indent role''
      <> space
      <> formatName indent blank name'
      <> foldMap
        ( \role' ->
            space <> formatRole indentation indent role'
        )
        roles
      <> newline
  CST.DeclError e -> absurd e
  where 
    indented = indent <> indentation
    lines = singleOrMultiline declaration


formatRole ::
  Indentation ->
  Indent ->
  (CST.SourceToken /\ CST.Role) ->
  String
formatRole indentation indent (sourceToken /\ role) = do
    formatSourceToken indentation indent sourceToken
      -- [TODO] Print the role

formatForeign ::
  Lines ->
  Indentation ->
  Indent ->
  CST.Foreign Void ->
  String
formatForeign span indentation indent' foreign'' = case foreign'' of
  CST.ForeignKind kind name' -> do
    formatSourceToken indent' blank kind
      <> formatName indent' space name'
  CST.ForeignValue labeled' -> do
    formatLabeledNameType indentation indent' labeled'
  CST.ForeignData data' labeled' -> do
    formatSourceToken indent' blank data'
      <> space
      <> formatLabeledNameKind indentation indent' labeled'

formatLabeledNameKind ::
  forall a.
  Indentation ->
  Indent ->
  CST.Labeled
    (CST.Name a)
    (CST.Type Void) ->
  String
formatLabeledNameKind indentation indent =
  formatLabeled
    indentation
    indent
    (formatName indent blank)
    (formatType indentation)

formatFixityFields ::
  Indent ->
  CST.FixityFields ->
  String
formatFixityFields indent { keyword: infix' /\ _, prec: precedence /\ _, operator } = do
    formatSourceToken indent blank infix'
      <> formatSourceToken indent space precedence
      <> space
      <> formatFixityOp indent operator
      
formatFixityOp ::
  Indent ->
  CST.FixityOp ->
  String
formatFixityOp indent fixityOp' = case fixityOp' of
  CST.FixityType type'' name' as op -> do
    formatSourceToken indent blank type''
      <> formatQualifiedName indent space name'
      <> formatSourceToken indent space as
      <> formatName indent space op
  CST.FixityValue name' as op -> do
    formatQualifiedName indent blank name'
      <> formatSourceToken indent space as
      <> formatName indent space op

formatInstance ::
  Indentation ->
  Indent ->
  CST.Instance Void ->
  String
formatInstance indentation indent (CST.Instance { head, body}) = 
  formatInstanceHead indentation indent head
      <> foldMap formatBindings body
  where
    formatBindings (whereClause /\ instanceBindings) =
      formatSourceToken indented space whereClause
        <> foldMap formatBinding instanceBindings
    formatBinding instanceBinding = 
      newline <> indent <> formatInstanceBinding indentation indented instanceBinding
    indented = indent <> indentation

formatInstanceHead ::
  Indentation ->
  Indent ->
  CST.InstanceHead Void ->
  String
formatInstanceHead indentation indent'' 
  instanceHead@
    { keyword: instance''
    , name: name'
    , separator: colons
    , constraints: constraints'
    , className 
    , types 
    } = do
    let 
        lines = MultiLine -- singleOrMultiline instanceHead
        indentTrick = (indent'' <> indentation)
        (indent /\ indent' /\ prefix) = case lines of
          MultiLine ->
            (indentTrick <> indentation) /\ indentTrick /\ (newline <> indentTrick)
          SingleLine ->
            (indent'' /\ indent'' /\ space)
        typePrefix = case head types of
          Just type'' -> case SingleLine of
            MultiLine -> newline <> indent
            SingleLine -> space
          Nothing -> space
    formatSourceToken indent'' blank instance''
      <> formatName indent'' space name'
      <> formatSourceToken indent'' space colons
      <> foldMap
        ( \(constraints /\ arrow) ->
            prefix
              <> formatOneOrDelimited
                lines
                indent'
                (formatConstraint indentation indent)
                constraints
              <> formatSourceToken indent' space arrow
        )
        constraints'
      <> formatQualifiedName indent' prefix className
      <> foldMap
        ( \type'' -> typePrefix <> formatType indentation indent type'')
        types
    
formatConstraint indentation indent _ = "constraint"

formatInstanceBinding ::
  Indentation ->
  Indent ->
  CST.InstanceBinding Void ->
  String
formatInstanceBinding indentation indent = case _ of
  CST.InstanceBindingSignature labeled -> do
    formatLabeledNameType indentation indent labeled
  CST.InstanceBindingName valueBindingFields -> do
    formatValueBindingFields indentation indent valueBindingFields

formatValueBindingFields ::
  Indentation ->
  Indent ->
  CST.ValueBindingFields Void ->
  String
formatValueBindingFields indentation indent { name, binders, guarded } =
    formatName indent blank name
      <> foldMap
        (\binder -> space <> formatBinder indentation indent binder)
        binders
      <> formatGuarded  indentation indent guarded

formatBinder ::
  Indentation ->
  Indent ->
  CST.Binder Void ->
  String
formatBinder indentation indent' binder = case binder of
  CST.BinderArray delimited' -> do
    formatArray
      lines
      indent'
      (formatBinder indentation indent')
      delimited'
  CST.BinderBoolean boolean _ -> do
    formatSourceToken indent' blank boolean
  CST.BinderChar char _ -> do
    formatSourceToken indent' blank char
  CST.BinderConstructor name' binders -> do
    let 
      indentTrick = indent' <> indentation
      indent /\ prefix = case lines of
          MultiLine ->
            indentTrick /\ (newline <> indentTrick)
          SingleLine ->
            indent' /\ space
    formatQualifiedName indent' blank name'
      <> foldMap
        ( \binder' ->
            prefix <> formatBinder indentation indent' binder'
        )
        binders
  CST.BinderNamed name' at binder' -> do
    formatName indent' blank name'
      <> formatSourceToken indent' blank at
      <> formatBinder indentation indent' binder'
  CST.BinderNumber negative number _ -> do
    foldMap (formatSourceToken indent' blank) negative
      <> formatSourceToken indent' blank number
  CST.BinderOp binder1 binders -> do
    formatBinder indent' prefix binder1 -- [TODO] Verify
      <> prefix 
      <> foldMap formatNamedBinder binders
    where
      indentTrick = indent' <> indentation
      indent /\ prefix = case lines of
          MultiLine ->
            indentTrick /\ (newline <> indentTrick)
          SingleLine ->
            indent' /\ space
      formatNamedBinder (name /\ binder) = 
        formatQualifiedName indent' prefix name
          <> prefix
          <> formatBinder indentation indent binder
  CST.BinderParens wrapped' -> do
    formatParens lines indentation indent' (formatBinder indentation) wrapped'
  CST.BinderRecord delimited' -> do
    formatRecord
      indent'
      ( formatRecordLabeled
          lines
          indentation
          indent'
          (formatBinder indentation)
      )
      delimited'
  CST.BinderString string _ -> do
    formatSourceToken indent' blank string
  CST.BinderTyped binder' colons type'' -> do
    let 
      indentTrick = indent' <> indentation
      indent /\ prefix = case lines of
          MultiLine ->
            indentTrick /\ (newline <> indentTrick)
          SingleLine ->
            indent' /\ space
    formatBinder indentation indent' binder'
      <> formatSourceToken indent' space colons
      <> prefix
      <> formatType  indentation indent type''
  CST.BinderVar name' ->
    formatName indent' blank name'
  CST.BinderWildcard wildcard ->
    formatSourceToken indent' blank wildcard
  CST.BinderInt negative int _  ->
    foldMap (formatSourceToken indent' blank) negative
      <> formatSourceToken indent' blank int
  CST.BinderError x -> absurd x
  where
    lines = singleOrMultiline binder

formatGuarded ::
  Indentation ->
  Indent ->
  CST.Guarded Void ->
  String
formatGuarded indentation indent' = case _ of
  CST.Guarded guardedExprs -> do
    let indent = indent' <> indentation
    foldMap
      ( \guardedExpr' ->
          (newline <> indent)
            <> formatGuardedExpr indentation indent guardedExpr'
      )
      guardedExprs
  CST.Unconditional separator where'' -> do
    formatSourceToken indent' space separator
      <> formatWhere indentation indent' where''

formatWhere ::
  Indentation ->
  Indent ->
  CST.Where Void ->
  String
formatWhere indentation indent' (CST.Where { expr: expr', bindings: letBindings''}) = do
    let indent = indent' <> indentation
    formatExprPrefix (singleOrMultiline expr') indentation indent' expr'
      <> foldMap
        ( \(where'' /\ letBindings') ->
            (newline <> indent)
              <> formatSourceToken indent blank where''
              <> foldMap
                (formatLetBinding indentation indent (newline <> indent) newline)
                (NE.init letBindings')
              <> formatLetBinding
                indentation
                indent
                (newline <> indent)
                blank
                (NE.last letBindings')
        )
        letBindings''

formatLetBinding ::
  Indentation ->
  Indent ->
  Prefix ->
  Suffix ->
  CST.LetBinding Void ->
  String
formatLetBinding indentation indent' prefix suffix = case _ of
  CST.LetBindingName valueBindingFields' -> do
    prefix
      <> formatValueBindingFields indentation indent' valueBindingFields'
      <> suffix
  CST.LetBindingPattern binder' equals where'' -> do
    prefix
      <> formatBinder indentation indent' binder'
      <> formatSourceToken indent' space equals
      <> formatWhere indentation indent' where''
      <> suffix
  CST.LetBindingSignature labeled' -> do
    prefix
      <> formatLabeledNameType indentation indent' labeled'
  CST.LetBindingError e -> absurd e

formatGuardedExpr ::
  Indentation ->
  Indent ->
  CST.GuardedExpr Void ->
  String
formatGuardedExpr indentation indent' expr = case expr of 
  CST.GuardedExpr { bar, patterns: patternGuards, separator, where:where'' } -> do
    let indent = indent' <> indentation
    formatSourceToken indent' blank bar
      <> space
      <> formatSeparated
        (singleOrMultiline expr)
        indent'
        space
        (formatPatternGuard (singleOrMultiline expr) indentation indent)
        patternGuards
      <> space
      <> formatSourceToken indent' blank separator
      <> formatWhere indentation indent' where''

formatPatternGuard ::
  Lines ->
  Indentation ->
  Indent ->
  CST.PatternGuard Void ->
  String
formatPatternGuard lines indentation indent' patternGuard@(CST.PatternGuard { binder: binder'', expr: expr'}) = do
    case binder'' of
      Just (binder' /\ arrow) ->
        formatBinder indentation indent' binder'
          <> formatSourceToken indent' space arrow
          <> formatExprPrefix lines indentation indent' expr'
      Nothing ->
        formatExpr indentation indent' expr'

formatExprPrefix lines indentation indent' expr =
  prefix <> formatExpr indentation indent' expr 
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
      CST.ExprNumber _  _-> indent' <> indentation
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
      CST.ExprBoolean _ _  -> newline <> indent
      CST.ExprCase _ -> space
      CST.ExprChar _ _  -> newline <> indent
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
    MultiLine -> multiLine
    SingleLine -> space

formatExpr ::
  Indentation ->
  Indent ->
  CST.Expr Void ->
  String
formatExpr indentation indent'' expr'' = case expr'' of
  CST.ExprAdo adoBlock' ->
    formatAdoBlock lines indentation indent'' adoBlock'
  CST.ExprApp expr1 expressions -> do -- [CHECK] Verify
    formatExpr indentation indent'' expr1
      <> foldMap (formatExprPrefix lines indentation indent'') expressions
  CST.ExprArray delimited' -> do
    let indent' = case lines of
          MultiLine ->
            indent'' <> indentation
          SingleLine ->
            indent''
    formatArray
      lines
      indent''
      (formatExpr indentation indent')
      delimited'
  CST.ExprBoolean boolean _ -> do
    formatSourceToken indent'' blank boolean
  CST.ExprCase caseOf' -> do
    formatCaseOf lines indentation indent'' caseOf'
  CST.ExprChar char _ -> do
    formatSourceToken indent'' blank char
  CST.ExprConstructor name' -> do
    formatQualifiedName indent'' blank name'
  CST.ExprDo doBlock' -> do
    formatDoBlock lines indentation indent'' doBlock'
  CST.ExprHole hole -> do
    formatName indent'' blank hole
  CST.ExprIdent name' -> do
    formatQualifiedName indent'' blank name'
  CST.ExprIf ifThenElse' -> do
    formatIfThenElse lines indentation indent'' ifThenElse'
  CST.ExprInfix expr1 expressions -> do
    formatExpr indentation indent'' expr1
      <> prefix'
      <> foldMap formatOne expressions 
    where
      indent0 = indent1 <> indentation
      indent1 = indent'' <> indentation
      (indent /\ indent' /\ prefix /\ prefix') = case lines of
        MultiLine ->
          ( indent0 /\
            indent1 /\
            (newline <> indent0) /\
            (newline <> indent1)
          )
        SingleLine ->
          (indent1 /\ indent'' /\ space /\ space)
      formatOne (wrapped' /\ expression) = 
        formatWrapped indent (formatExpr indentation indent') wrapped'
        <> prefix
        <> formatExpr indentation indent expression   
  CST.ExprLambda lambda' -> do
    formatLambda lines indentation indent'' lambda'
  CST.ExprLet letIn' -> do
    formatLetIn lines indentation indent'' letIn'
  CST.ExprNegate negative expr' -> do
    formatSourceToken indent'' blank negative
      <> formatExpr indentation indent'' expr'
  CST.ExprNumber number _ -> do
    formatSourceToken indent'' blank number
  CST.ExprOp expr1 expressions -> do
    "hih"
    -- let 
    --   indentTrick = indent'' <> indentation
    --   indent /\ indent' /\ prefix = case lines of
    --       MultiLine ->
    --         (indentTrick <> indentation) /\ indentTrick /\ (newline <> indentTrick)
    --       SingleLine ->
    --         indentTrick /\ indent'' /\ space
    -- formatExpr indentation indent'' expr1
    --   <> formatQualifiedName indent' prefix op
    --   <> space
    --   <> formatExpr indentation indent expr2
  CST.ExprOpName name' -> do
    formatQualifiedName indent'' blank name'
  CST.ExprParens wrapped' -> do
    formatParens lines indentation indent'' (formatExpr indentation) wrapped'
  CST.ExprRecord delimited' -> do
    formatRecord
      indent''
      (formatRecordLabeled
          lines
          indentation
          indent''
          (formatExpr indentation)
      )
      delimited'
  CST.ExprRecordAccessor recordAccessor' -> do
    formatRecordAccessor lines indentation indent'' recordAccessor'
  CST.ExprRecordUpdate expr' delimitedNonEmpty' -> do
    let 
      indentTrick = indent'' <> indentation
      indent' /\ prefix = case lines of
          MultiLine ->
            indentTrick /\ (newline <> indentTrick)
          SingleLine ->
            indent'' /\ space
    formatExpr indentation indent'' expr'
      <> prefix
      <> formatRecordNonEmpty
        indent'
        (formatRecordUpdate lines indentation indent')
        delimitedNonEmpty'
  CST.ExprSection section -> 
    formatSourceToken indent'' blank section
  CST.ExprString string _ -> 
    formatSourceToken indent'' blank string
  CST.ExprTyped expr' colons type'' -> do
    let 
      indentTrick = indent'' <> indentation
      indent' /\ prefix = case lines of
          MultiLine ->
            indentTrick /\ (newline <> indentTrick)
          SingleLine ->
            indent'' /\ space
    formatExpr indentation indent'' expr'
      <> formatSourceToken indent'' space colons
      <> prefix
      <> formatType indentation indent' type''
  CST.ExprInt int _ -> formatSourceToken indent'' blank int
  CST.ExprError e -> absurd e
  where
    lines = singleOrMultiline expr''

formatRecord ::
  forall a.
  Indent ->
  (a -> String) ->
  CST.Delimited a ->
  String
formatRecord indent f record'' = case record'' of
  CST.Wrapped { open, value: Nothing, close } -> do
    formatSourceToken indent blank open
      <> formatSourceToken indent blank close
  CST.Wrapped { open, value: Just record',  close } ->
    formatRecordNonEmpty
      indent
      f
      (CST.Wrapped { open, value: record', close })

formatRecordAccessor ::
  Lines ->
  Indentation ->
  Indent ->
  CST.RecordAccessor Void ->
  String
formatRecordAccessor lines indentation indent' recordAccessor' = case recordAccessor' of
  { expr: expr', dot, path } -> do
    let 
      indentTrick = indent' <> indentation
      indent /\ prefix = case lines of
          MultiLine ->
            indentTrick /\ (newline <> indentTrick)
          SingleLine ->
            indent' /\ blank
    formatExpr indentation indent expr'
      <> prefix
      <> formatSourceToken indent blank dot
      <> formatSeparated
        lines -- (lines.separated SourceRange.label path)
        indent
        blank
        (formatName indent' blank)
        path

-- [CHECK] Verify lines stuff
formatRecordUpdate ::
  Lines -> 
  Indentation ->
  Indent ->
  CST.RecordUpdate Void ->
  String
formatRecordUpdate lines indentation indent' recordUpdate' = case recordUpdate' of
  CST.RecordUpdateBranch label' delimitedNonEmpty' -> do
    let 
      indentTrick = indent' <> indentation
      indent /\ prefix = case lines of
          MultiLine ->
            indentTrick /\ (newline <> indentTrick)
          SingleLine ->
            indent' /\ space
    formatName indent' blank label'
      <> prefix
      <> formatRecordNonEmpty
        indent
        (formatRecordUpdate lines indentation indent)
        delimitedNonEmpty'
  CST.RecordUpdateLeaf label' equals expr' -> do
    let 
      indentTrick =indent' <> indentation
      indent /\ prefix = case lines of
          MultiLine ->
            indentTrick /\ (newline <> indentTrick)
          SingleLine ->
            indent' /\ space
    
    formatName indent' blank label'
      <> formatSourceToken indent' space equals
      <> prefix
      <> formatExpr indentation indent expr'

-- [TODO] No idea what's happening here with RangeOf
formatRecordLabeled ::
  forall a.
  Lines ->
  Indentation ->
  Indent ->
  (Indent -> a -> String) ->
  CST.RecordLabeled a ->
  String
formatRecordLabeled lines indentation indent' f recordLabeled' = case recordLabeled' of
  CST.RecordPun name' -> do
    formatName indent' blank name'
  CST.RecordField label' colon a -> do
    let indentTrick = indent' <> indentation <> indentation
        indent /\ prefix = case lines of
          MultiLine ->
            indentTrick /\ (newline <> indentTrick)
          SingleLine ->
            indent' /\ space
    formatName indent' blank label'
      <> formatSourceToken indent' blank colon
      <> prefix
      <> f indent a


formatRecordNonEmpty ::
  forall a.
  Indent ->
  (a -> String) ->
  CST.DelimitedNonEmpty a ->
  String
formatRecordNonEmpty indent formatValue record' = do
  let 
    (before /\ after) = case lines of 
      MultiLine ->
        (blank /\ blank)
      SingleLine ->
        (space /\ space)
  formatWrapped
    indent
    ( \separated' ->
        before
          <> formatSeparated lines indent space formatValue separated' -- [CHECK] is the lines correct
          <> after
    )
    record'
  where
    lines = singleOrMultiline record'

formatLetIn ::
  Lines ->
  Indentation ->
  Indent ->
  CST.LetIn Void ->
  String
formatLetIn lines indentation indent' letIn' = case letIn' of
  { keyword: let'
  , bindings: letBindings
  , in: in'
  , body: expr''
  } -> do
    let 
      indentTrick = indent' <> indentation
      inPrefix /\ indent /\ prefix = case lines of
        MultiLine ->
          (newline <> indent') /\ indentTrick /\ (newline <> indentTrick)
        SingleLine ->
          space /\ indent' /\ space
    formatSourceToken indent' blank let'
      <> foldMap
        (formatLetBinding indentation indent prefix newline)
        (NE.init letBindings)
      <> formatLetBinding
        indentation
        indent
        prefix
        blank
        (NE.last letBindings)
      <> inPrefix
      <> formatSourceToken indent' blank in'
      <> prefix
      <> formatExpr indentation indent expr''

formatLambda ::
  Lines ->
  Indentation ->
  Indent ->
  CST.Lambda Void ->
  String
formatLambda lines indentation indent' lambda' = case lambda' of
  { symbol: reverseSolidus, binders, arrow, body: expr' } -> do
    formatSourceToken indent' blank reverseSolidus
      <> foldMap (\binder' -> formatBinder indentation indent' binder' <> space) binders
      <> formatSourceToken indent' blank arrow
      <> formatExprPrefix lines indentation indent' expr'

formatExprPrefixElseIf ::
  Lines ->
  Indentation ->
  Indent ->
  CST.Expr Void ->
  String
formatExprPrefixElseIf lines indentation indent expr' =
  case expr' of
    CST.ExprAdo _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprApp _ _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprArray _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprBoolean _ _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprCase _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprChar _ _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprConstructor _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprDo _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprHole _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprIdent _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprIf _ -> space <> formatExpr indentation indent expr'
    CST.ExprInfix _ _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprLambda _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprLet _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprNegate _ _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprNumber _ _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprOp _ _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprOpName _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprParens _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprRecord _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprRecordAccessor _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprRecordUpdate _ _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprSection _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprString _ _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprTyped _ _ _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprInt _ _ -> formatExprPrefix lines indentation indent expr'
    CST.ExprError e -> absurd e

formatIfThenElse ::
  Lines ->
  Indentation ->
  Indent ->
  CST.IfThenElse Void ->
  String
formatIfThenElse lines indentation indent ifThenElse' = case ifThenElse' of
  { keyword : if', cond, then: then', true: true', else: else', false: false' } -> do
    let prefix = case lines of
          MultiLine ->
            newline <> indent
          SingleLine ->
            space
    formatSourceToken indent blank if'
      <> space
      <> formatExpr indentation indent cond
      <> space
      <> formatSourceToken indent blank then'
      <> formatExprPrefix lines indentation indent true'
      <> prefix
      <> formatSourceToken indent blank else'
      <> formatExprPrefixElseIf lines indentation indent false'

formatDoBlock ::
  Lines ->
  Indentation ->
  Indent ->
  CST.DoBlock Void ->
  String
formatDoBlock lines indentation indent' doBlock' = case doBlock' of
  { keyword: do', statements: doStatements } -> do
    let 
      indentTrick = indent' <> indentation
      indent /\ prefix = case lines of
          MultiLine ->
            indentTrick /\ (newline <> indentTrick)
          SingleLine ->
            indent' /\ space
    formatSourceToken indent' blank do'
      <> foldMap
        ( \doStatement' ->
            prefix
              <> formatDoStatement  indentation indent doStatement'
        )
        doStatements


formatDoStatement ::
  forall a.
  Indentation ->
  Indent ->
  CST.DoStatement Void ->
  String
formatDoStatement indentation indent' doStatement' = case doStatement' of
  CST.DoBind binder' arrow expr' -> do
    formatBinder indentation indent' binder'
      <> formatSourceToken indent' space arrow
      <> formatExprPrefix (singleOrMultiline doStatement') indentation indent' expr'
  CST.DoDiscard expr' -> do
    formatExpr indentation indent' expr'
  CST.DoLet let' letBindings -> do
    let indent = indent' <> indentation
    formatSourceToken indent' blank let'
      <> foldMap
        (formatLetBinding indentation indent (newline <> indent) newline)
        (NE.init letBindings)
      <> formatLetBinding
        indentation
        indent
        (newline <> indent)
        blank
        (NE.last letBindings)
  CST.DoError e -> absurd e

formatCaseOf ::
  Lines ->
  Indentation ->
  Indent ->
  CST.CaseOf Void ->
  String
formatCaseOf lines indentation indent' caseOf' = case caseOf' of
  { keyword: case', head, of: of', branches } -> do
    let 
      indentTrick = indent' <> indentation
      indent /\ prefix = case lines of
        MultiLine -> indentTrick /\ (newline <> indentTrick)
        SingleLine -> indent' /\ space
    formatSourceToken  indent' blank case'
      <> space
      <> formatSeparated
        (singleOrMultiline head)
        indent
        space
        (formatExpr indentation indent)
        head
      <> space
      <> formatSourceToken indent' blank of'
      <> foldMap
        ( \(binders /\ guarded') ->
            prefix
              <> formatSeparated
                (singleOrMultiline binders)
                indent
                space
                (formatBinder indentation indent)
                binders
              <> formatGuarded indentation indent guarded'
        )
        branches

formatArray ::
  forall a . 
  Lines ->
  Indent ->
  (a -> String) ->
  CST.Delimited a ->
  String
formatArray lines indent formatValue array'' = case array'' of
  CST.Wrapped { open, value: Nothing, close } -> do
    formatSourceToken indent blank open
      <> formatSourceToken indent blank close
  CST.Wrapped { open, value: Just array', close } ->
    formatArrayNonEmpty
      lines
      indent
      formatValue
      (CST.Wrapped { open, value: array', close})

formatArrayNonEmpty ::
  forall a.
  Lines ->
  Indent ->
  (a -> String) ->
  CST.DelimitedNonEmpty a ->
  String
formatArrayNonEmpty lines indent formatValue  = 
  formatWrapped indent formatOne
  where
    before /\ after = case lines of
      MultiLine -> blank /\ blank
      SingleLine -> space /\ space
    formatOne separated =
      before
        <> formatSeparated lines indent space formatValue separated
        <> after

formatAdoBlock ::
  Lines ->
  Indentation ->
  Indent ->
  CST.AdoBlock Void ->
  String
formatAdoBlock lines indentation indent' { keyword, statements: doStatements, in: in', result: expr'} = do
    let 
      { indent, prefix } = case lines of
        MultiLine -> do
          let indent = indent' <> indentation
          { indent, prefix: newline <> indent } 
        SingleLine -> { indent: indent', prefix: space }
    
    formatSourceToken indent' blank keyword
      <> foldMap
        ( \doStatement' -> prefix <> formatDoStatement indentation indent doStatement')
        doStatements
      <> prefix
      <> formatSourceToken indent' blank in'
      <> space
      <> formatExpr indentation indent expr'

formatLabeledNameType ::
  forall a.
  Indentation ->
  Indent ->
  CST.Labeled (CST.Name a) (CST.Type Void) ->
  String
formatLabeledNameType indentation indent =
  formatLabeled
    indentation
    indent
    (formatName indent blank)
    (formatType indentation)

formatClassHead
  :: forall a
   . RangeOf a
  => Lines
  -> Indentation
  -> Indent
  -> CST.ClassHead a
  -> String
formatClassHead lines indentation indent { keyword, super, name, vars, fundeps } = do
    formatSourceToken indented blank keyword
      <> foldMap formatSuper super
      <> formatName indent space name
      <> foldMap formatTypeVariableBinding vars
      <> foldMap formatFunctionalDependency fundeps
  where
    { indented, prefix } = case lines of
      MultiLine -> do
        let indented = indent <> indentation
        { indented, prefix: newline <> indented } 
      SingleLine -> { indented: indent, prefix: space }
    
    formatTypeVariableBinding binding = 
      space <> formatTypeVarBinding indentation indent binding
    
    formatFunctionalDependency (bar /\ classFundeps) =
      formatSourceToken indent space bar
        <> space
        <> formatSeparated
            lines
            indent
            space
            (formatClassFundep indent)
            classFundeps

    formatSuper (constraints /\ arrow) =
      prefix
        <> formatOneOrDelimited 
             lines
             indented
             (formatType indentation indent)
             constraints
        <> formatSourceToken indent space arrow

formatClassFundep ::
  Indent ->
  CST.ClassFundep ->
  String
formatClassFundep indent = case _ of
  CST.FundepDetermined arrow names ->
    formatSourceToken indent blank arrow
      <> foldMap (formatName indent space) names
  CST.FundepDetermines names arrow moreNames -> do
    foldMap (\name -> formatName indent blank name <> space) names
      <> formatSourceToken indent blank arrow
      <> foldMap (formatName indent space) moreNames

formatOneOrDelimited ::
  forall a.
  RangeOf a =>
  Lines ->
  Indent ->
  (a -> String) ->
  CST.OneOrDelimited a ->
  String
formatOneOrDelimited lines indent formatValue oneOrDelimited = case oneOrDelimited of
  CST.One a -> formatValue a
  CST.Many delimitedNonEmpty ->
    formatDelimitedNonEmpty lines indent formatValue delimitedNonEmpty

formatDelimitedNonEmpty ::
  forall a.
  RangeOf a =>
  Lines ->
  Indent ->
  (a -> String) ->
  CST.DelimitedNonEmpty a ->
  String
formatDelimitedNonEmpty lines indent formatValue = 
  formatWrapped indent (formatSeparated lines indent space formatValue)

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
        MultiLine -> do
          let indented = indent <> indentation
          { indented, prefix: newline <> indented } 
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
formatTypeVarBinding indentation indent = case _ of
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
  -- sourceToken  indent prefix qualifiedName'

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
type Suffix = String

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