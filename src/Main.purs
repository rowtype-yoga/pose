module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array.NonEmpty (foldMap1)
import Data.Either (Either(..), either)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Class.Console as Log
import Effect.Exception (error)
import Format (format)
import Node.Encoding (Encoding(..))
import Node.FS.Async (readTextFile)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)

foreign import argv :: Array String

main :: Effect Unit
main = do
  case argv of
    [] -> Log.error "Please supply a file"
    [_] -> Log.error "Please supply a file"
    [_, _] -> Log.error "Please supply a file"
    [_, _, file] -> do
      readTextFile UTF8 file case _ of 
        Left e -> throwError e
        Right input -> 
          case parseAndFormat' input of
            Left bad -> throwError (error bad)
            Right ok -> log ok
    _ -> Log.error "Please only supply one argument"

parseAndFormat :: String -> String
parseAndFormat = either identity identity <<< parseAndFormat'
  
parseAndFormat' :: String -> Either String String
parseAndFormat' = parseModule >>> case  _ of
    ParseSucceeded cst ->
      -- Right $ printModule cst
      Right $ format cst
    ParseSucceededWithErrors _cst errors -> do
      Left $ foldMap1 (\{error, position} -> printParseError error <> " at " <> show position) errors
    ParseFailed _errors -> do
      Left "Parse Failed"
