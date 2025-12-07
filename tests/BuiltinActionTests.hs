module BuiltinActionTests (tests) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Quirky.Actions.Builtin
import Quirky.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Builtin Action Tests"
  [ parseJsonTests
  , jsonExtractTests
  , parseHtmlTests
  , regexMatchTests
  , compareNumberTests
  , fileStatTests
  ]

-- | Test parseJsonAction
parseJsonTests :: TestTree
parseJsonTests = testGroup "ParseJson Action"
  [ testCase "parses valid JSON" $ do
      let config = object ["input" .= String "{\"key\": \"value\"}"]
      result <- runExceptT $ parseJsonAction config
      case result of
        Right (Object obj) -> do
          KM.lookup "status" obj @?= Just (String "ok")
          case KM.lookup "data" obj of
            Just (Object dataObj) ->
              KM.lookup "key" dataObj @?= Just (String "value")
            _ -> assertFailure "data field is not an object"
        _ -> assertFailure "Expected successful parse"

  , testCase "fails on invalid JSON" $ do
      let config = object ["input" .= String "{invalid json}"]
      result <- runExceptT $ parseJsonAction config
      case result of
        Left (ExecutionError _) -> return ()  -- Expected
        _ -> assertFailure "Should fail on invalid JSON"

  , testCase "handles missing input field" $ do
      let config = object []
      result <- runExceptT $ parseJsonAction config
      case result of
        Left (ConfigError _) -> return ()  -- Expected
        _ -> assertFailure "Should fail with missing input"

  , testCase "parses complex JSON" $ do
      let jsonStr = "{\"users\": [{\"name\": \"Alice\", \"age\": 30}]}"
      let config = object ["input" .= String jsonStr]
      result <- runExceptT $ parseJsonAction config
      case result of
        Right (Object obj) -> do
          case KM.lookup "data" obj of
            Just (Object dataObj) ->
              case KM.lookup "users" dataObj of
                Just (Array _) -> return ()  -- Successfully parsed array
                _ -> assertFailure "users field is not an array"
            _ -> assertFailure "data field is not an object"
        _ -> assertFailure "Expected successful parse"
  ]

-- | Test jsonExtractAction
jsonExtractTests :: TestTree
jsonExtractTests = testGroup "JsonExtract Action"
  [ testCase "extracts simple field" $ do
      let jsonData = object ["name" .= String "Alice", "age" .= Number 30]
      let config = object ["json" .= jsonData, "path" .= String "name"]
      result <- runExceptT $ jsonExtractAction config
      case result of
        Right (Object obj) -> do
          KM.lookup "status" obj @?= Just (String "ok")
          case KM.lookup "data" obj of
            Just (Object dataObj) ->
              KM.lookup "value" dataObj @?= Just (String "Alice")
            _ -> assertFailure "data field is not an object"
        _ -> assertFailure "Expected successful extraction"

  , testCase "extracts nested field" $ do
      let jsonData = object ["user" .= object ["name" .= String "Bob"]]
      let config = object ["json" .= jsonData, "path" .= String "user.name"]
      result <- runExceptT $ jsonExtractAction config
      case result of
        Right (Object obj) ->
          case KM.lookup "data" obj of
            Just (Object dataObj) ->
              KM.lookup "value" dataObj @?= Just (String "Bob")
            _ -> assertFailure "data field is not an object"
        _ -> assertFailure "Expected successful extraction"

  , testCase "returns error for missing path" $ do
      let jsonData = object ["name" .= String "Alice"]
      let config = object ["json" .= jsonData, "path" .= String "missing"]
      result <- runExceptT $ jsonExtractAction config
      case result of
        Right (Object obj) ->
          KM.lookup "status" obj @?= Just (String "error")
        _ -> assertFailure "Expected error status"

  , testCase "handles missing json field" $ do
      let config = object ["path" .= String "name"]
      result <- runExceptT $ jsonExtractAction config
      case result of
        Left (ConfigError _) -> return ()  -- Expected
        _ -> assertFailure "Should fail with missing json field"
  ]

-- | Test parseHtmlAction
parseHtmlTests :: TestTree
parseHtmlTests = testGroup "ParseHtml Action"
  [ testCase "extracts text from tag" $ do
      let html = "<div><p>Hello World</p></div>"
      let config = object ["html" .= String html, "selector" .= String "p"]
      result <- runExceptT $ parseHtmlAction config
      case result of
        Right (Object obj) -> do
          case KM.lookup "results" obj of
            Just (Array results) -> do
              V.length results @?= 1
              results V.! 0 @?= String "Hello World"
            _ -> assertFailure "results field is not an array"
        _ -> assertFailure "Expected successful parse"

  , testCase "extracts multiple elements" $ do
      let html = "<div><p>First</p><p>Second</p></div>"
      let config = object ["html" .= String html, "selector" .= String "p"]
      result <- runExceptT $ parseHtmlAction config
      case result of
        Right (Object obj) ->
          case KM.lookup "matches" obj of
            Just (Number n) -> n @?= 2
            _ -> assertFailure "matches field is not a number"
        _ -> assertFailure "Expected successful parse"

  , testCase "handles no matches" $ do
      let html = "<div><p>Hello</p></div>"
      let config = object ["html" .= String html, "selector" .= String "span"]
      result <- runExceptT $ parseHtmlAction config
      case result of
        Right (Object obj) ->
          case KM.lookup "matches" obj of
            Just (Number n) -> n @?= 0
            _ -> assertFailure "matches field is not a number"
        _ -> assertFailure "Expected successful parse"
  ]

-- | Test regexMatchAction
regexMatchTests :: TestTree
regexMatchTests = testGroup "RegexMatch Action"
  [ testCase "matches simple pattern" $ do
      let config = object
            [ "input" .= String "Hello World"
            , "pattern" .= String "World"
            ]
      result <- runExceptT $ regexMatchAction config
      case result of
        Right (Object obj) -> do
          KM.lookup "status" obj @?= Just (String "ok")
          case KM.lookup "data" obj of
            Just (Object dataObj) ->
              KM.lookup "matched" dataObj @?= Just (Bool True)
            _ -> assertFailure "data field is not an object"
        _ -> assertFailure "Expected successful match"

  , testCase "returns error on no match" $ do
      let config = object
            [ "input" .= String "Hello World"
            , "pattern" .= String "Goodbye"
            ]
      result <- runExceptT $ regexMatchAction config
      case result of
        Right (Object obj) -> do
          KM.lookup "status" obj @?= Just (String "error")
          case KM.lookup "data" obj of
            Just (Object dataObj) ->
              KM.lookup "matched" dataObj @?= Just (Bool False)
            _ -> assertFailure "data field is not an object"
        _ -> assertFailure "Expected error status"

  , testCase "captures groups" $ do
      let config = object
            [ "input" .= String "Version: 1.2.3"
            , "pattern" .= String "Version: ([0-9.]+)"
            , "capture_group" .= Number 1
            ]
      result <- runExceptT $ regexMatchAction config
      case result of
        Right (Object obj) ->
          case KM.lookup "data" obj of
            Just (Object dataObj) ->
              KM.lookup "match" dataObj @?= Just (String "1.2.3")
            _ -> assertFailure "data field is not an object"
        _ -> assertFailure "Expected successful match"

  , testCase "handles invalid capture group" $ do
      let config = object
            [ "input" .= String "test"
            , "pattern" .= String "test"
            , "capture_group" .= Number 10
            ]
      result <- runExceptT $ regexMatchAction config
      case result of
        Right (Object obj) ->
          case KM.lookup "data" obj of
            Just (Object dataObj) ->
              -- Out of bounds capture group returns empty string
              KM.lookup "match" dataObj @?= Just (String "")
            _ -> assertFailure "data field is not an object"
        _ -> assertFailure "Expected successful match"
  ]

-- | Test compareNumberAction
compareNumberTests :: TestTree
compareNumberTests = testGroup "CompareNumber Action"
  [ testCase "greater than comparison (true)" $ do
      let config = object
            [ "value" .= Number 10
            , "threshold" .= Number 5
            , "operator" .= String ">"
            ]
      result <- runExceptT $ compareNumberAction config
      case result of
        Right (Object obj) -> do
          KM.lookup "status" obj @?= Just (String "ok")
          case KM.lookup "data" obj of
            Just (Object dataObj) ->
              KM.lookup "result" dataObj @?= Just (Bool True)
            _ -> assertFailure "data field is not an object"
        _ -> assertFailure "Expected successful comparison"

  , testCase "greater than comparison (false)" $ do
      let config = object
            [ "value" .= Number 3
            , "threshold" .= Number 5
            , "operator" .= String ">"
            ]
      result <- runExceptT $ compareNumberAction config
      case result of
        Right (Object obj) -> do
          KM.lookup "status" obj @?= Just (String "error")
          case KM.lookup "data" obj of
            Just (Object dataObj) ->
              KM.lookup "result" dataObj @?= Just (Bool False)
            _ -> assertFailure "data field is not an object"
        _ -> assertFailure "Expected error status"

  , testCase "less than comparison" $ do
      let config = object
            [ "value" .= Number 3
            , "threshold" .= Number 5
            , "operator" .= String "<"
            ]
      result <- runExceptT $ compareNumberAction config
      case result of
        Right (Object obj) ->
          KM.lookup "status" obj @?= Just (String "ok")
        _ -> assertFailure "Expected successful comparison"

  , testCase "equals comparison" $ do
      let config = object
            [ "value" .= Number 5
            , "threshold" .= Number 5
            , "operator" .= String "=="
            ]
      result <- runExceptT $ compareNumberAction config
      case result of
        Right (Object obj) ->
          KM.lookup "status" obj @?= Just (String "ok")
        _ -> assertFailure "Expected successful comparison"

  , testCase "not equals comparison" $ do
      let config = object
            [ "value" .= Number 3
            , "threshold" .= Number 5
            , "operator" .= String "!="
            ]
      result <- runExceptT $ compareNumberAction config
      case result of
        Right (Object obj) ->
          KM.lookup "status" obj @?= Just (String "ok")
        _ -> assertFailure "Expected successful comparison"

  , testCase "handles missing fields" $ do
      let config = object ["value" .= Number 5]
      result <- runExceptT $ compareNumberAction config
      case result of
        Left (ConfigError _) -> return ()  -- Expected
        _ -> assertFailure "Should fail with missing threshold"
  ]

-- | Test fileStatAction
fileStatTests :: TestTree
fileStatTests = testGroup "FileStat Action"
  [ testCase "returns exists=false for non-existent file" $ do
      let config = object ["path" .= String "/tmp/nonexistent-quirky-test-file-12345"]
      result <- runExceptT $ fileStatAction config
      case result of
        Right (Object obj) ->
          KM.lookup "exists" obj @?= Just (Bool False)
        _ -> assertFailure "Expected successful stat"

  , testCase "handles missing path field" $ do
      let config = object []
      result <- runExceptT $ fileStatAction config
      case result of
        Left (ConfigError _) -> return ()  -- Expected
        _ -> assertFailure "Should fail with missing path"
  ]
