module TemplatingTests (tests) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Quirky.Engine (applyTemplating)
import Quirky.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Templating Tests"
  [ simpleTemplateTests
  , nestedPathTests
  , jsonRefTests
  , arrayTemplatingTests
  , edgeCaseTests
  ]

-- | Test simple template substitution
simpleTemplateTests :: TestTree
simpleTemplateTests = testGroup "Simple Template Substitution"
  [ testCase "replaces simple string template" $ do
      let ctx = Map.fromList [("name", String "Alice")]
      let input = String "Hello {{ name }}"
      case applyTemplating ctx input of
        Right (String result) -> result @?= "Hello Alice"
        _ -> assertFailure "Template substitution failed"

  , testCase "replaces number template" $ do
      let ctx = Map.fromList [("count", Number 42)]
      let input = String "Count: {{ count }}"
      case applyTemplating ctx input of
        Right (String result) -> result @?= "Count: 42.0"
        _ -> assertFailure "Number substitution failed"

  , testCase "replaces boolean template" $ do
      let ctx = Map.fromList [("flag", Bool True)]
      let input = String "Flag: {{ flag }}"
      case applyTemplating ctx input of
        Right (String result) -> result @?= "Flag: true"
        _ -> assertFailure "Boolean substitution failed"

  , testCase "keeps original if variable not found" $ do
      let ctx = Map.empty
      let input = String "Hello {{ missing }}"
      case applyTemplating ctx input of
        Right (String result) -> result @?= "Hello {{missing}}"
        _ -> assertFailure "Should keep original template"

  , testCase "handles multiple templates in one string" $ do
      let ctx = Map.fromList [("first", String "John"), ("last", String "Doe")]
      let input = String "Name: {{ first }} {{ last }}"
      case applyTemplating ctx input of
        Right (String result) -> result @?= "Name: John Doe"
        _ -> assertFailure "Multiple template substitution failed"

  , testCase "handles templates with whitespace" $ do
      let ctx = Map.fromList [("name", String "Alice")]
      let input = String "Hello {{  name  }}"
      case applyTemplating ctx input of
        Right (String result) -> result @?= "Hello Alice"
        _ -> assertFailure "Whitespace handling failed"
  ]

-- | Test nested path lookups
nestedPathTests :: TestTree
nestedPathTests = testGroup "Nested Path Lookups"
  [ testCase "accesses nested object property" $ do
      let ctx = Map.fromList
            [ ("user", object ["name" .= String "Alice", "age" .= Number 30])
            ]
      let input = String "User: {{ user.name }}"
      case applyTemplating ctx input of
        Right (String result) -> result @?= "User: Alice"
        _ -> assertFailure "Nested access failed"

  , testCase "accesses deeply nested property" $ do
      let ctx = Map.fromList
            [ ("data", object
                [ "response" .= object
                    [ "user" .= object ["name" .= String "Bob"]
                    ]
                ])
            ]
      let input = String "Name: {{ data.response.user.name }}"
      case applyTemplating ctx input of
        Right (String result) -> result @?= "Name: Bob"
        _ -> assertFailure "Deep nesting failed"

  , testCase "returns original if nested path not found" $ do
      let ctx = Map.fromList [("data", object [])]
      let input = String "Value: {{ data.missing.path }}"
      case applyTemplating ctx input of
        Right (String result) -> result @?= "Value: {{data.missing.path}}"
        _ -> assertFailure "Should keep original"
  ]

-- | Test json_ref functionality
jsonRefTests :: TestTree
jsonRefTests = testGroup "JSON Reference Injection"
  [ testCase "injects referenced JSON value" $ do
      let ctx = Map.fromList
            [ ("api_response", object ["status" .= String "ok", "code" .= Number 200])
            ]
      let input = object ["json_ref" .= String "api_response", "other" .= String "value"]
      case applyTemplating ctx input of
        Right (Object result) -> do
          -- The object should have "json" with the api_response value, not json_ref
          case KM.lookup "json" result of
            Just _ -> return ()
            Nothing -> assertFailure "json field not found after json_ref substitution"
        _ -> assertFailure "json_ref substitution failed"

  , testCase "fails if json_ref not found in context" $ do
      let ctx = Map.empty
      let input = object ["json_ref" .= String "missing"]
      case applyTemplating ctx input of
        Left (TemplatingError _) -> return ()  -- Expected failure
        _ -> assertFailure "Should fail with TemplatingError"

  , testCase "handles nested json_ref path" $ do
      let ctx = Map.fromList
            [ ("response", object ["data" .= object ["user" .= String "Alice"]])
            ]
      let input = object ["json_ref" .= String "response.data"]
      case applyTemplating ctx input of
        Right (Object _) -> return ()  -- Successfully injected
        _ -> assertFailure "Nested json_ref failed"
  ]

-- | Test array templating
arrayTemplatingTests :: TestTree
arrayTemplatingTests = testGroup "Array Templating"
  [ testCase "applies templating to array elements" $ do
      let ctx = Map.fromList [("value", String "test")]
      let input = Array $ V.fromList [String "before {{ value }}", String "after {{ value }}"]
      case applyTemplating ctx input of
        Right (Array result) -> do
          V.length result @?= 2
          result V.! 0 @?= String "before test"
          result V.! 1 @?= String "after test"
        _ -> assertFailure "Array templating failed"

  , testCase "applies templating to nested objects in array" $ do
      let ctx = Map.fromList [("name", String "Alice")]
      let input = Array $ V.fromList [object ["user" .= String "{{ name }}"]]
      case applyTemplating ctx input of
        Right (Array result) -> V.length result @?= 1
        _ -> assertFailure "Nested object in array failed"
  ]

-- | Test edge cases
edgeCaseTests :: TestTree
edgeCaseTests = testGroup "Edge Cases"
  [ testCase "handles empty string" $ do
      let ctx = Map.fromList [("key", String "value")]
      let input = String ""
      case applyTemplating ctx input of
        Right (String result) -> result @?= ""
        _ -> assertFailure "Empty string failed"

  , testCase "handles malformed template (no closing braces)" $ do
      let ctx = Map.empty
      let input = String "{{ incomplete"
      case applyTemplating ctx input of
        Right (String result) -> result @?= "{{ incomplete"
        _ -> assertFailure "Should keep malformed template"

  , testCase "handles consecutive templates" $ do
      let ctx = Map.fromList [("a", String "A"), ("b", String "B")]
      let input = String "{{ a }}{{ b }}"
      case applyTemplating ctx input of
        Right (String result) -> result @?= "AB"
        _ -> assertFailure "Consecutive templates failed"

  , testCase "handles null value in context" $ do
      let ctx = Map.fromList [("val", Null)]
      let input = String "Value: {{ val }}"
      case applyTemplating ctx input of
        Right (String result) -> result @?= "Value: "
        _ -> assertFailure "Null handling failed"

  , testCase "preserves non-string values" $ do
      let ctx = Map.empty
      let input = Number 123
      case applyTemplating ctx input of
        Right (Number n) -> n @?= 123
        _ -> assertFailure "Number preservation failed"
  ]
