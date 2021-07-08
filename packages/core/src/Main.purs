module Main where

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
import Format (format)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)
import OS (stdinAsString)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import Settings (Settings, defaultSettings)

main :: Effect Unit
main = do
  input <- stdinAsString
  launchAff_ $ parseAndFormatOrThrow defaultSettings input


parseAndFormatOrThrow :: Settings -> String -> Aff Unit
parseAndFormatOrThrow settings =
  parseAndFormat' settings
    >>> case _ of
        Left bad -> throwError (error bad)
        Right formatted -> log formatted

parseAndFormat :: Settings -> String -> String
parseAndFormat settings = either identity identity <<< parseAndFormat' settings

parseAndFormat' :: Settings -> String -> Either String String
parseAndFormat' settings =
  parseModule
    >>> case _ of
        ParseSucceeded cst -> Right $ format settings cst
        ParseSucceededWithErrors _cst errors ->
          Left
            $ foldMap1
                ( \{ error, position } ->
                    printParseError error <> " at " <> show position
                )
                errors
        ParseFailed _errors -> do
          Left "Parse Failed"

