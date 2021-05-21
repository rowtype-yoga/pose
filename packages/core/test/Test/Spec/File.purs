module Test.Spec.File where

import Prelude
import Control.Monad.Except (runExceptT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Data.Newtype (unwrap)
import Data.UUID (genUUID, toString)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Ex
import Foreign (Foreign)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Simple.JSON (class ReadForeign, parseJSON, read)

fileAsString ∷ ∀ m. MonadEffect m ⇒ String → m String
fileAsString path = liftEffect $ readTextFile UTF8 path

fileAsForeign ∷ ∀ m. MonadEffect m ⇒ String → m Foreign
fileAsForeign path = do
  fileContent ← fileAsString path
  liftEffect
    $ either
        (\s → Ex.throwException <<< Ex.error $ "Content of file " <> path <> " is not valid Json: " <> (show s))
        pure
        (unwrap $ runExceptT $ (parseJSON fileContent))

fileAs ∷ ∀ m r. MonadEffect m ⇒ ReadForeign r ⇒ String → m r
fileAs path = do
  json ← fileAsForeign path
  liftEffect
    $ either
        (\s → Ex.throwException <<< Ex.error $ "Content of file " <> path <> " is JSON but not of the expected type: " <> (show s))
        pure
        (read json)

fileAsJson ∷ ∀ m. MonadEffect m ⇒ String → m Json
fileAsJson path = do
  fileContent ← fileAsString path
  liftEffect
    $ either
        (\s → Ex.throwException <<< Ex.error $ "Content of file " <> path <> " is not valid Json: " <> s)
        pure
        (jsonParser fileContent)

writeToTmpFile ∷ ∀ m. MonadEffect m ⇒ String → m String
writeToTmpFile contents = do
  fileNameUUID ← liftEffect genUUID
  writeToNamedTmpFile (toString fileNameUUID) contents

writeToNamedTmpFile ∷ ∀ m. MonadEffect m ⇒ String → String → m String
writeToNamedTmpFile fileName contents = do
  let path = "/tmp/" <> fileName <> ".html"
  liftEffect $ writeTextFile UTF8 path contents
  pure path
