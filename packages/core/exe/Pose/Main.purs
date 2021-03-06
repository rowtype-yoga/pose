module Pose.Main where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Array.NonEmpty (foldMap1)
import Data.Either (Either(..), either)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import Effect.Class.Console as Log
import Effect.Exception (error)
import Pose.Format (format)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import Pose.Settings (Settings, defaultSettings)

foreign import argv ∷ Array String

foreign import stdinFile ∷ FilePath

main ∷ Effect Unit
main =
  launchAff_ do
    stdinContent ← readTextFile UTF8 stdinFile
    if stdinContent /= "" then
      parseAndFormatOrThrow defaultSettings stdinContent
    else case Array.drop 2 argv of
      [ fileName ] → do
        readTextFile UTF8 fileName >>= parseAndFormatOrThrow defaultSettings
      _ → Log.error "Please only supply one argument"

parseAndFormatOrThrow ∷ Settings → String → Aff Unit
parseAndFormatOrThrow settings =
  parseAndFormat' settings
    >>> case _ of
        Left bad → throwError (error bad)
        Right formatted → log formatted

parseAndFormat ∷ Settings → String → String
parseAndFormat settings = either identity identity <<< parseAndFormat' settings

parseAndFormat' ∷ Settings → String → Either String String
parseAndFormat' settings =
  parseModule
    >>> case _ of
        ParseSucceeded cst → Right $ format settings cst
        ParseSucceededWithErrors _cst errors →
          Left
            $ foldMap1
                ( \{ error, position } →
                    printParseError error <> " at " <> show position
                )
                errors
        ParseFailed _errors → do
          Left "Parse Failed"
