module Test.Main where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Main (parseAndFormat)
import Node.FS.Aff (readdir)
import Node.Path (FilePath, basename)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions.Diff (Actual(..), GoldenFile(..), shouldBeGolden)
import Test.Spec.File (fileAsString)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =  launchAff_ do
  -- fileNames <- A.take 2 <$> readdir original
  fileNames <- Array.take 1025 <$> readdir original
  runSpec [specReporter] (testGolden fileNames)
  pure unit

testGolden :: Array FilePath -> Spec Unit
testGolden fileNames = do
  describe "Golden Tests" do
    describe "formats" do
      for_ fileNames $ testOne golden
  
testOne :: FilePath -> FilePath -> Spec Unit
testOne expected fullFileName = 
  it fileName do
    content <- liftEffect $ fileAsString $ original <> fileName
    let actual = parseAndFormat content
    -- log $ "\n==============================================\n" <> actual <> "\n==============================================\n"
    Actual actual `shouldBeGolden` GoldenFile (expected <> fileName)
  where 
    fileName = basename fullFileName

golden :: FilePath
golden = "testfiles/golden/"

original :: FilePath 
original = "testfiles/original/"

   