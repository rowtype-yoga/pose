module Pose.Format.Indentations
  ( Indentation
  , Lines(..)
  , Prefix
  , Suffix
  , blank
  , commentIsSpaceOrLineFeed
  , hasNonWhitespaceLeadingComment
  , hasNonWhitespaceTrailingComment
  , isPrecededByBlankLines
  , isPrecededByNewline
  , newline
  , precedingEmptyLines
  , rangeOfInstanceHead
  , rangeOfPatternGuard
  , rangeOfSeparated
  , singleOrMultiline
  , singleOrMultilineBetween
  , singleOrMultilineBetweenSourceRanges
  , singleOrMultilineFromRange
  , sourceTokenIsPrecededByNewline
  , space
  , unsafeFirstTokenOf
  , unsafeLastTokenOf
  ) where

import Prelude
import Data.Array (any)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst, snd)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST.Range (class RangeOf, class TokensOf, rangeOf, tokensOf)
import PureScript.CST.Range.TokenList as TokensOf
import PureScript.CST.Types as CST

data Lines
  = SingleLine
  | MultipleLines

singleOrMultiline ∷ ∀ a. RangeOf a ⇒ a → Lines
singleOrMultiline value = singleOrMultilineFromRange (rangeOf value)

singleOrMultilineFromRange ∷ CST.SourceRange → Lines
singleOrMultilineFromRange { start, end } =
  if start.line == end.line then
    SingleLine
  else
    MultipleLines

singleOrMultilineBetween ∷ ∀ a b. RangeOf a ⇒ RangeOf b ⇒ a → b → Lines
singleOrMultilineBetween a b = do
  let { start } = rangeOf a
  let { end } = rangeOf b
  singleOrMultilineFromRange { start, end }

singleOrMultilineBetweenSourceRanges ∷ CST.SourceRange → CST.SourceRange → Lines
singleOrMultilineBetweenSourceRanges { start } { end } =
  singleOrMultilineFromRange { start, end }

rangeOfInstanceHead ∷ CST.InstanceHead Void → Lines
rangeOfInstanceHead { keyword, className, types } = do
  let
    { end } = case Array.last types of
      Nothing → rangeOf className
      Just ty → rangeOf ty
  singleOrMultilineFromRange
    { start: keyword.range.start
    , end
    }

rangeOfPatternGuard ∷ (CST.PatternGuard Void) → CST.SourceRange
rangeOfPatternGuard (CST.PatternGuard { binder, expr }) = do
  let
    { start } = maybe (rangeOf expr) (fst >>> rangeOf) binder
    { end } = rangeOf expr
  { start, end }

rangeOfSeparated ∷
  ∀ a.
  (a → CST.SourceRange) →
  CST.Separated a →
  CST.SourceRange
rangeOfSeparated f separated = case separated of
  CST.Separated { head, tail } → do
    let headRange = f head
    let tailRange = maybe headRange f (Array.last tail <#> snd)
    { start: headRange.start, end: tailRange.end }

precedingEmptyLines ∷ Array (CST.Comment CST.LineFeed) → Int
precedingEmptyLines =
  Array.takeWhile commentIsSpaceOrLineFeed
    >>> map toLines
    >>> sum
  where
  toLines = case _ of
    CST.Line _ n → n
    _ → 0

isPrecededByBlankLines ∷ ∀ t. TokensOf t ⇒ t → Boolean
isPrecededByBlankLines x =
  precedingEmptyLines (unsafeFirstTokenOf x).leadingComments > 1

unsafeFirstTokenOf ∷ ∀ a. TokensOf a ⇒ a → CST.SourceToken
unsafeFirstTokenOf x = case TokensOf.head (tokensOf x) of
  Just sourceToken → sourceToken
  Nothing → unsafeCrashWith "Unexpectedly got no tokens"

unsafeLastTokenOf ∷ ∀ a. TokensOf a ⇒ a → CST.SourceToken
unsafeLastTokenOf x = case Array.last (TokensOf.toArray (tokensOf x)) of
  Just sourceToken → sourceToken
  Nothing → unsafeCrashWith "Unexpectedly got no tokens"

isPrecededByNewline ∷ ∀ t. TokensOf t ⇒ t → Boolean
isPrecededByNewline x = case TokensOf.head (tokensOf x) of
  Just sourceToken → sourceTokenIsPrecededByNewline sourceToken
  Nothing → unsafeCrashWith "Unexpectedly got no tokens"

sourceTokenIsPrecededByNewline ∷ CST.SourceToken → Boolean
sourceTokenIsPrecededByNewline { leadingComments } =
  precedingEmptyLines leadingComments > 0

hasNonWhitespaceTrailingComment ∷ CST.SourceToken → Boolean
hasNonWhitespaceTrailingComment =
  _.trailingComments
    >>> any case _ of
        CST.Comment "" → false -- Don't think this should happen
        CST.Comment _ → true
        _ → false

hasNonWhitespaceLeadingComment ∷ CST.SourceToken → Boolean
hasNonWhitespaceLeadingComment =
  _.leadingComments
    >>> any case _ of
        CST.Comment "" → false -- Don't think this should happen
        CST.Comment _ → true
        _ → false

commentIsSpaceOrLineFeed ∷ ∀ c. CST.Comment c → Boolean
commentIsSpaceOrLineFeed = case _ of
  CST.Space _ → true
  CST.Line _ _ → true
  _ → false

type Prefix
  = String

type Suffix
  = String

type Indentation
  = String

newline ∷ String
newline = "\n"

blank ∷ String
blank = ""

space ∷ String
space = " "
