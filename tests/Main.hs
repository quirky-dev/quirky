module Main (main) where

import Test.Tasty
import qualified ConfigParsingTests
import qualified ActionTests
import qualified BuiltinActionTests
import qualified AlertTests
import qualified TemplatingTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Quirky Tests"
  [ ConfigParsingTests.tests
  , ActionTests.tests
  , BuiltinActionTests.tests
  , AlertTests.tests
  , TemplatingTests.tests
  ]
