module Main where

import Prelude

import Data.Array.NonEmpty (foldMap1)
import Data.Either (Either(..), either)
import Format (format)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)

parse :: String -> String
parse = either identity identity <<< parse'
  
parse' :: String -> Either String String
parse' = parseModule >>> case  _ of
    ParseSucceeded cst ->
      -- Right $ printModule cst
      Right $ format cst
    ParseSucceededWithErrors _cst errors -> do
      Left $ foldMap1 (\{error, position} -> printParseError error <> " at " <> show position) errors
    ParseFailed _errors -> do
      Left "Parse Failed"
