module Pose.Format.CommentHack
  ( wrapTrailingComment
  , unwrapTrailingComments
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags)
import Data.String.Regex.Flags as Flags
import Data.String.Regex.Unsafe (unsafeRegex)
import PureScript.CST.Types as CST

commentHack ∷ String
commentHack = "👁️" <> "_" <> "👁️"

openCommentHack ∷ String
openCommentHack = "{-" <> commentHack

closeCommentHack ∷ String
closeCommentHack = commentHack <> "-}"

-- Deal with line comments on types and aligned arrows
-- (notice the arrow getting formatted into the comment):
-- avoids:              String -- line comment ->
-- by turning it into:  String {- line comment -} ->
wrapTrailingComment ∷ CST.Comment Void → CST.Comment Void
wrapTrailingComment c = case c of
  CST.Comment full → case String.stripPrefix (String.Pattern "--") full of
    Just words → CST.Comment (openCommentHack <> words <> " " <> closeCommentHack)
    Nothing → c
  _ → c

unwrapTrailingComments ∷ String → String
unwrapTrailingComments =
  Regex.replace endOfLineCommentsRegex "--$1"
    >>> Regex.replace nonEndOfLineCommentsRegex ""

endOfLineCommentsRegex ∷ Regex
endOfLineCommentsRegex = unsafeRegex ("{-" <> commentHack <> "([^👁]*) " <> commentHack <> "-}$") flags

nonEndOfLineCommentsRegex ∷ Regex
nonEndOfLineCommentsRegex = unsafeRegex commentHack flags

flags ∷ RegexFlags
flags = Flags.multiline <> Flags.global
