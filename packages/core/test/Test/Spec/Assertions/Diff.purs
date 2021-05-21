module Test.Spec.Assertions.Diff
  ( shouldHaveNoDiff
  , shouldBeGolden
  , Actual(..)
  , Expected(..)
  , GoldenFile(..)
  , (=|=)
  ) where

import Prelude
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, writeTextFile)
import Test.Spec.Assertions (fail)
import Test.Spec.File (fileAsString, writeToNamedTmpFile)

foreign import getPrettyHtml ∷ String → String → String → String

newtype Actual
  = Actual String

derive instance ntActual ∷ Newtype Actual _

newtype Expected
  = Expected String

derive instance ntExpected ∷ Newtype Expected _

newtype GoldenFile
  = GoldenFile String

derive instance ntGoldenFile ∷ Newtype GoldenFile _

infix 4 shouldHaveNoDiff as =|=

shouldBeGolden ∷ Actual → GoldenFile → Aff Unit
shouldBeGolden actual@(Actual content) (GoldenFile path) = do
  exist ← exists path
  if exist then do
    expected ← fileAsString path
    shouldHaveNoDiff' actual path (Expected expected)
  else do
    writeTextFile UTF8 path content
    fail $ "Recreated " <> path

shouldHaveNoDiff' ∷ Actual → String → Expected → Aff Unit
shouldHaveNoDiff' (Actual actual) expectedName (Expected expected) =
  unless (actual == expected) do
    let
      before =
        """<!DOCTYPE html>
      <html>
        <head>
          <meta charset="utf-8"/>
          <!-- CSS -->
          <link rel="stylesheet" type="text/css" href="https://cdn.jsdelivr.net/npm/diff2html/bundles/css/diff2html.min.css" />
          <!-- Javascripts -->
          <script type="text/javascript" src="https://cdn.jsdelivr.net/npm/diff2html/bundles/js/diff2html.min.js"></script>
          <style>
          .d2h-code-side-linenumber, .d2h-code-line-prefix {
            -webkit-touch-callout: none;
            -webkit-user-select: none;
            -khtml-user-select: none;
            -moz-user-select: none;
            -ms-user-select: none;
            user-select: none;
          }
          </style>
          <title>DIFF!</title>
        </head>
        <body>
          <div>
      """
    let after = "</div></body></html>"
    let diff = getPrettyHtml expectedName actual expected
    path ←
      writeToNamedTmpFile
        ( expectedName
            # replaceAll (Pattern "/") (Replacement "-")
            # replaceAll (Pattern "\\") (Replacement "_")
            # (_ <> "-actual")
        )
        $ before
        <> diff
        <> after
    fail <<< show $ "The diff is saved here: file://" <> path

shouldHaveNoDiff ∷ Actual → Expected → Aff Unit
shouldHaveNoDiff a = shouldHaveNoDiff' a "expected"
